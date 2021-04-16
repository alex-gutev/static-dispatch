;;;; sbcl.lisp
;;;;
;;;; Copyright 2021 Alexander Gutev
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use,
;;;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;;;; copies of the Software, and to permit persons to whom the
;;;; Software is furnished to do so, subject to the following
;;;; conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; SBCL specific static dispatch

(in-package :static-dispatch)


;;; Compiler Transform

(defmacro enable-static-dispatch (&rest names)
  "Enable static dispatching for generic functions with names NAMES."

  `(progn ,@(mapcar (curry #'list 'enable-static-dispatch%) names)))

(defmacro enable-static-dispatch% (name)
  (labels ((make-transforms (method prior)
	     ;; Generate the DEFTRANSFORM forms for a given METHOD and
	     ;; given list of the PRIOR method specializers.

	     (with-slots (specializers) method
	       (let ((types (make-types specializers prior)))
		 (mapcar #'make-transform types))))

	   (make-transform (types)
	     ;; Generate the DEFTRANSFORM form for a given list of
	     ;; TYPES

	     (with-gensyms (args)
	       `(sb-c:deftransform ,name ((&rest ,args) (,@types &rest *) * :policy (> speed safety))
		  (or (static-overload ',name ',args ',types)
		      (sb-c::give-up-ir1-transform)))))

	   (make-types (types prior)
	     ;; For each T type in TYPES, return a specializer list
	     ;; with the T replaced with the negation of the
	     ;; corresponding type in each PRIOR specializer list, for
	     ;; which the prefix of specializer list matches TYPES.

	     (let ((ts (positions t types)))
	       (if ts
		   (mapcar (rcurry #'make-negated-types types prior) ts)
		   (list types))))

	   (make-negated-types (pos types prior)
	     ;; Substitute the type at index POS with the negation of
	     ;; all the corresponding types of all PRIOR specializer
	     ;; lists which have the same prefix as TYPES.

	     (let ((prior (filter-prior pos types prior))
		   (types (copy-seq types)))

	       (if prior
		   (aprog1 (copy-seq types)
		     (setf (elt it pos) `(not (or ,@prior))))
		   types)))

	   (filter-prior (pos types prior)
	     ;; Remove prior specializer lists from PRIOR which do not
	     ;; have the same prefix as TYPES in the range [0, POS).

	     (let ((prefix (subseq types 0 pos)))
	       (->> (remove-if-not (curry #'is-prefix prefix) prior)
		    (mapcar (rcurry #'elt pos))
		    (remove-duplicates))))

	   (is-prefix (prefix types)
	     ;; Check if a specializer list TYPES matches a given
	     ;; PREFIX.

	     (starts-with-subseq prefix types :test #'type-match))

	   (type-match (a b)
	     ;; Check that two types match

	     (match* (a b)
	       ((t _) t)
	       ((_ t) t)
	       ((_ _) (equal a b))))

	   (sort-methods (methods)
	     (sort methods #'method< :key #'car))

	   (method< (m1 m2)
	     (ematch* (m1 m2)
	       (((list _ s1)
		 (list _ s2))

		(specializer< s1 s2)))))

    (let* ((gf (fdefinition name))
	   (precedence (precedence-order (generic-function-lambda-list gf) (generic-function-argument-precedence-order gf)))
	   (methods (-<> (aand (gf-methods name) (hash-table-alist it))
			 (order-method-specializers precedence)
			 (sort-methods)
			 (remove-duplicates :key #'cadar :test #'equal)
			 (mapcar #'cdr <>))))
      (unless methods
	(warn "No methods found for generic function ~a" name))

      `(progn
	 (sb-c:defknown ,name * * () :overwrite-fndb-silently t)

	 ,@ (reverse
	     (loop for method in methods
		for transform = (make-transforms method prior)
		collect (specializers method) into prior
		append transform))))))

(defun static-dispatch (whole &optional env)
  "A no-op on SBCL since static dispatching is handled by the compiler
   transforms, rather than compiler macros."

  (declare (ignore env))
  whole)

(defun static-overload (name args types)
  (when (fboundp name)
    (let ((*current-gf* name)
	  (gf (fdefinition name)))

      (let* ((precedence (precedence-order (generic-function-lambda-list gf) (generic-function-argument-precedence-order gf)))
	     (types (order-by-precedence precedence types))
	     (methods (-<> (aand (gf-methods name) (hash-table-alist it))
			   (order-method-specializers precedence)
			   (applicable-methods types)
			   (sort-methods)
			   (mapcar #'cdr <>))))
	(when methods
	  `(progn
	     (static-dispatch-test-hook)
	     ,(inline-methods methods args nil types)))))))


;;; Utilities

(defun positions (item seq)
  "Return a list containing the positions of the elements of sequence
   SEQ which are EQL to ITEM."

  (loop
     for i from 0
     for elem in seq
     when (eql item elem) collect i))
