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

  (let ((*method-functions* (copy-hash-table *method-functions*)))
    `(progn ,@(mapcar #'make-enable-static-dispatch names))))

(defun make-enable-static-dispatch (name)
  (ematch name
    ((or (list (and (or :inline :overload) dispatch-type)
	       name)
	 name)

     (labels ((sort-methods (methods)
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
	   (simple-style-warning "Could not enable static dispatch for: ~a. No methods found." name))

	 `(progn
	    ,@(when (eq dispatch-type :overload)
		(make-static-overload-functions name))

	    ,@(when (eq dispatch-type :inline)
		(list (make-remove-method-function-names name)))

	    (sb-c:defknown ,name * * () :overwrite-fndb-silently t)

	    ,@(reverse (mappend (curry #'make-transform name) methods))))))))

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


;;; Generating DEFTRANSFORM's

(defun make-transform (name method)
  "Generate a DEFTRANSFORM definition for statically dispatching a generic function method.

   NAME is the name of the generic function.

   METHOD is the `METHOD-INFO' object for which to create a IR1
   transform.

   Returns a list of DEFTRANSFORM forms."

  (labels ((t-specializer? (specializer)
	     (match specializer
	       ((list* t _)
		t)

	       ((list* _ rest)
		(t-specializer? rest)))))

    (with-slots (specializers) method
      (if (t-specializer? specializers)
	  (make-default-transforms name method)
	  (make-typed-transforms name method)))))

(defun make-typed-transforms (name method)
  "Generate a DEFTRANSFORM definition for statically dispatching a method with typed specializers

   This should only be used when all the method's specializers are specific types, not T.

   NAME is the name of the generic function.

   METHOD is the `METHOD-INFO' object for which to create a IR1
   transform.

   Returns a list of DEFTRANSFORM forms."

  (with-slots (lambda-list specializers) method
    (let ((type-list (lambda-list->type-list lambda-list specializers)))
      (with-gensyms (args node)
	(list* (->> (make-transform-body name nil specializers node)
		    (make-destructure-transform name method (or type-list specializers) node))

	       (when type-list
		 (->> (make-transform-body name args specializers node)
		      (make-arglist-transform name method args node)
		      list)))))))

(defun make-default-transforms (name method)
  "Generate a DEFTRANSFORM definition for statically dispatching a method with T specializers

   This should be used when some or all of the method's specializers
   are T.

   NAME is the name of the generic function.

   METHOD is the `METHOD-INFO' object for which to create an IR1
   transform.

   Returns a list of DEFTRANSFORM forms."

  (with-slots (lambda-list specializers) method
    (let ((type-list (lambda-list->type-list lambda-list specializers)))
      (with-gensyms (args node)
	(list* (->> (make-default-transform-body name nil specializers node)
		    (make-destructure-transform name method (or type-list specializers) node))

	       (when type-list
		 (->> (make-default-transform-body name args specializers node)
		      (make-arglist-transform name method args node)
		      list)))))))

(defun make-destructure-transform (name method type-list node body)
  "Generate a DEFTRANSFORM with a specific lambda list.

   This generates a transform which is only applied if the structure
   implied by the lambda-list matches the arguments given.

   NAME is the name of the generic function.

   METHOD is the `METHOD-INFO' object for which to create an IR1
   transform.

   TYPE-LIST is the parameter type specification list.

   NODE is the name of the variable to which the compilation node is
   bound.

   BODY is the form which implements the transform."

  (with-slots (lambda-list specializers) method
    `(sb-c:deftransform ,name (,lambda-list ,type-list * :policy (> speed safety) :node ,node)
       ,(format nil "Inline ~s method ~s" name specializers)

       (let ((*full-arg-list-form* ,(make-reconstruct-arg-list lambda-list))
	     (*call-args* ,(make-reconstruct-static-arg-list lambda-list)))
	 ,body))))

(defun make-arglist-transform (name method args node body)
  "Generate a DEFTRANSFORM with a non-specific lambda list.

   This generates a transform with and (&rest ...) lambda-list, which
   will required manual destructuring further on.

   NAME is the name of the generic function.

   METHOD is the `METHOD-INFO' object for which to create an IR1
   transform.

   ARGS is the name of the &REST variable to which the entire argument
   list is bound.

   NODE is the name of the variable to which the compilation node is
   bound.

   BODY is the form which implements the transform."

  (with-slots (specializers) method
    `(sb-c:deftransform ,name ((&rest ,args) (,@specializers &rest *) * :policy (> speed safety) :node ,node)
       ,(format nil "Inline ~s method ~s" name specializers)

       ,body)))

(defun lambda-list->type-list (lambda-list specializers)
  "Convert a lambda-list to a type specifier list.

   LAMBDA-LIST is the lambda-list.

   SPECIALIZERS is the list of type specializers of the required
   arguments of the lambda-list."

  (multiple-value-bind (required optional rest key allow-other-keys)
      (parse-ordinary-lambda-list lambda-list)

    (assert (length= specializers required))

    (when (or optional key)
      (append specializers
	      (when optional '(&optional))
	      (loop for opt in optional collect '*)
	      (when rest '(&rest *))
	      (when (or key allow-other-keys) '(&key))
	      (loop for ((keyword)) in key collect `(,keyword *))
	      (when allow-other-keys '(&allow-other-keys))))))

(defun make-default-transform-body (name args specializers node)
  "Generate the body of a default (untyped) transform.

   NAME is the name of the generic function.

   ARGS is the argument list specifier.

   SPECIALIZERS is the specializer list of the method.

   NODE is the name of the variable to which the compilation node is
   bound.

   Returns a form implementing the transform."

  `(or
    (when (should-transform? ,node ',specializers)
      (static-overload ',name ',args ',specializers ,node))
    (sb-c::give-up-ir1-transform)))

(defun make-transform-body (name args specializers node)
  "Generate the body of a typed transform.

   NAME is the name of the generic function.

   ARGS is the argument list specifier.

   SPECIALIZERS is the specializer list of the method.

   NODE is the name of the variable to which the compilation node is
   bound.

   Returns a form implementing the transform."

  `(or (static-overload ',name ',args ',specializers ,node)
       (sb-c::give-up-ir1-transform)))

(defun make-reconstruct-arg-list (lambda-list)
  "Generate a form which generates a form that reconstructs an argument list.

   The generated form is expected to be used inside a DEFTRANSFORM
   where each lambda-list variable is bound to the compilation entity
   or NIL if not provided.

   LAMBDA-LIST is the lambda-list from which to reconstruct the
   argument list.

   Returns a form that when evaluated produces another form that
   reconstructs the argument list."

  (multiple-value-bind (required optional rest key)
      (parse-ordinary-lambda-list lambda-list)

    (labels ((make-required (required)
	       (ematch required
		 ((list* var rest)
		  ``(cons ,',var ,,(make-required rest)))

		 (nil
		  (make-optional optional))))

	     (make-optional (optional)
	       (ematch optional
		 ((list* (list var _ _) rest)
		  `(when ,var `(cons ,',var ,,(make-optional rest))))

		 (nil
		  (make-rest rest))))

	     (make-rest (rest)
	       (if rest `',rest (make-key key)))

	     (make-key (key)
	       (ematch key
		 ((list* (list (list key var) _ _) rest)
		  `(when ,var `(list* ,',key ,',var ,,(make-key rest))))

		 (nil))))

      (make-required required))))

(defun make-reconstruct-static-arg-list (lambda-list)
  "Generate a form that reconstructs an argument list.

   The generated form is expected to be used inside a DEFTRANSFORM
   where each lambda-list variable is bound to the compilation entity
   or NIL if not provided.

   LAMBDA-LIST is the lambda-list from which to reconstruct the
   argument list.

   Returns a form that when evaluated returns the static argument
   list."

  (multiple-value-bind (required optional rest key)
      (parse-ordinary-lambda-list lambda-list)

    (labels ((make-required (required)
	       (ematch required
		 ((list* var rest)
		  `(cons ',var ,(make-required rest)))

		 (nil
		  (make-optional optional))))

	     (make-optional (optional)
	       (ematch optional
		 ((list* (list var _ _) rest)
		  `(when ,var (cons ',var ,(make-optional rest))))

		 (nil
		  (make-rest rest))))

	     (make-rest (rest)
	       (if rest
		   `(loop
		       repeat (length ,rest)
		       for cons = ',rest then `(cdr ,cons)
		       collect `(car ,cons))
		   (make-key key)))

	     (make-key (key)
	       (ematch key
		 ((list* (list (list key var) _ _) rest)
		  `(when ,var (list* ',key ',var ,(make-key rest))))

		 (nil))))

      (make-required required))))


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
