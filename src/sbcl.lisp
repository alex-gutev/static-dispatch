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
  (labels ((make-transform (method)
	     (with-slots (specializers) method
	       (if (t-specializer? specializers)
		   (make-default-transform method)
		   (make-typed-transform method))))

	   (make-typed-transform (method)
	     ;; Make DEFTRANSFORM form for specific types
	     (with-slots (specializers) method
	       (with-gensyms (args node)
		 `(sb-c:deftransform ,name ((&rest ,args) (,@specializers &rest *) * :policy (> speed safety) :node ,node)

		    (or (static-overload ',name ',args ',specializers ,node)
			(sb-c::give-up-ir1-transform))))))

	   (make-default-transform (method)
	     ;; Make DEFTRANSFORM form for specializers with T types.
	     ;; A check is made to make sure the arguments are not of
	     ;; type T before applying the transform.

	     (with-slots (specializers) method
	       (with-gensyms (args node)
		 `(sb-c:deftransform ,name ((&rest ,args) (,@specializers &rest *) * :policy (> speed safety) :node ,node)
		    (or
		     (when (should-transform? ,node ',specializers)
		       (static-overload ',name ',args ',specializers ,node))
		     (sb-c::give-up-ir1-transform))))))

	   (t-specializer? (specializer)
	     (match specializer
	       ((list* t _)
		t)

	       ((list* _ rest)
		(t-specializer? rest))))

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

	 ,@(reverse (mapcar #'make-transform methods))))))

(defun should-transform? (node specializers)
  "Checks whether the transform should be applied for a given compilation NODE.

   If SPECIALIZERS contains a T specializer and the corresponding
   argument type is of type T, which implies the type is undetermined
   the transform is not performed. Otherwise it is performed."

  (let ((t-type (sb-kernel:specifier-type t)))
    (flet ((is-t? (specializer type)
	     (and (eq specializer t)
		  (sb-kernel:type= type t-type))))

      (with-accessors ((args sb-c::combination-args))
	  node

	(when (and (sb-c::combination-p node)
		   (every #'sb-c::lvar-p args))

	  (->> (mapcar #'sb-c::lvar-type args)
	       (notany #'is-t? specializers)))))))


;;; Static Dispatching

(defun static-dispatch (whole &optional env)
  "A no-op on SBCL since static dispatching is handled by the compiler
   transforms, rather than compiler macros."

  (declare (ignore env))
  whole)

(defun static-overload (name args types node)
  (when (fboundp name)
    (let ((*current-gf* name)
	  (gf (fdefinition name)))

      (let* ((precedence (precedence-order (generic-function-lambda-list gf) (generic-function-argument-precedence-order gf)))
	     (types (order-by-precedence precedence types))
	     (methods (-<> (aand (gf-methods name) (hash-table-alist it))
			   (order-method-specializers precedence)
			   (applicable-methods types nil)
			   (sort-methods)
			   (mapcar #'cdr <>))))
	(when methods
	  `(progn
	     (static-dispatch-test-hook)
	     ,(inline-methods methods args (sb-c:policy node (not (or (eql speed 3) (eql safety 0)))) types)))))))


;;; Utilities

(defun positions (item seq)
  "Return a list containing the positions of the elements of sequence
   SEQ which are EQL to ITEM."

  (loop
     for i from 0
     for elem in seq
     when (eql item elem) collect i))
