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


;;; Generating DEFTRANSFORM's

(define-constant +static-dispatch-policy+
    '(and (= speed 3) (< safety 3) (< debug 3) (< compilation-speed 3))
  :test 'equal
  :documentation
  "Optimization policy at which static dispatching is performed.")

(defun make-static-dispatch (name lambda-list specializers)
  (let ((specializers (substitute '* 'eql specializers :key #'ensure-car))
        (type-list (lambda-list->type-list lambda-list specializers))
        (required (parse-ordinary-lambda-list lambda-list)))

    (with-gensyms (node args types)
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (unless (sb-c::info :function :info ',name)
             (sb-c:defknown ,name * * nil :overwrite-fndb-silently t)))

         (locally
             (declare (sb-ext:muffle-conditions style-warning))

           (handler-bind ((style-warning #'muffle-warning))
             ,@(when type-list
                 `((sb-c:deftransform ,name ((&rest ,args) (,@specializers &rest *) * :policy ,+static-dispatch-policy+ :node ,node)
                     ,(format nil "Inline ~s method ~s" name specializers)

                     (let ((types
                            ,(make-dispatch-type-list
                              (loop for i from 0 below (length required)
                                 collect `(nth ,i ,args))
                              node)))

                       (or (static-overload ',name ',args ',type-list types ,node)
                           (sb-c::give-up-ir1-transform))))))

             ,(multiple-value-bind (gensym-lambda-list bindings)
                  (gensym-lambda-list lambda-list)

                (let ((required (parse-ordinary-lambda-list gensym-lambda-list)))
                  `(sb-c:deftransform ,name (,gensym-lambda-list ,(or type-list specializers) * :policy ,+static-dispatch-policy+ :node ,node)
                     ,(format nil "Inline ~s method ~s" name specializers)

                     (let ((*full-arg-list-form* ,(make-reconstruct-arg-list gensym-lambda-list))
                           (*call-args* ,(make-reconstruct-static-arg-list gensym-lambda-list))
                           (*argument-map* ',bindings)
                           (,types ,(make-dispatch-type-list required node)))

                       (or (static-overload ',name nil ',type-list ,types ,node)
                           (sb-c::give-up-ir1-transform))))))))))))

(defun make-dispatch-type-list (args node)
  "Generate a form which generates the argument type list.

   ARGS are the transform arguments to include in the type list.

   NODE is the compilation node argument."

  `(let ((*handle-sb-lvars* t))
     (list
      ,@(loop for arg in args
           collect
             `(nth-form-type ,arg (sb-c::node-lexenv ,node))))))

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

(defun gensym-lambda-list (lambda-list)
  "Replace the variables in a lambda-list with gensyms.

   LAMBDA-LIST: The lambda-list.

   Returns two values:

     1. The new lambda-list with the variables replaced by gensyms.

     2. The list of bindings, as if to LET, binding the original
        variables to the corresponding gensym'd variables."

  (multiple-value-bind (required optional rest key allow-other-keys aux keyp)
      (parse-ordinary-lambda-list lambda-list)

    (let ((bindings))
      (labels
          ((gensym-arg (arg)
             (let ((g (gensym (symbol-name arg))))
               (push (list arg g) bindings)
               g))

           (wrap-init-form (init)
             (when init
               `(symbol-macrolet ,bindings
                  ,init)))

           (gensym-optional (arg)
             (destructuring-bind (name init sp) arg
               (list (gensym-arg name)
                     (wrap-init-form init)
                     (when sp (gensym-arg sp)))))

           (gensym-key (arg)
             (destructuring-bind ((key name) init sp) arg
               (list (list key (gensym-arg name))
                     (wrap-init-form init)
                     (when sp (gensym-arg sp)))))

           (gensym-aux (arg)
             (destructuring-bind (name init) arg
               (list (gensym-arg name)
                     (wrap-init-form init)))))

        (let ((required (mapcar #'gensym-arg required))
              (optional (mapcar #'gensym-optional optional))
              (rest (when rest (gensym-arg rest)))
              (key (mapcar #'gensym-key key))
              (aux (mapcar #'gensym-aux aux)))

          (values
           (unparse-lambda-list required optional rest key allow-other-keys aux keyp)
           bindings))))))


;;; Static Dispatching

(defun static-dispatch (whole &optional env)
  "A no-op on SBCL since static dispatching is handled by the compiler
   transforms, rather than compiler macros."

  (declare (ignore env))
  whole)

(defun static-overload (name args type-list types node)
  (handler-case
      (progn
        (unless (and (fboundp name)
                     (typep (fdefinition name) 'generic-function))
          (error "~s is not a generic function." name))

        (let* ((gf (fdefinition name))
               (methods (compute-applicable-methods% name types)))

          (when (should-apply? methods type-list)
            (if methods
                (inline-call gf methods args types (sb-c:policy node (> safety 0)) (should-call-fn? name (sb-c::node-lexenv node)))
                (error "No applicable methods for argument types: ~s" types)))))

    (combin-destructure-args-error (e)
      (when args
        (dispatch-warn (sb-c::node-lexenv node) name e)
        nil))

    (error (e)
      (dispatch-warn (sb-c::node-lexenv node) name e)
      nil)))

(defun should-apply? (methods type-list)
  "Check whether the current transform should be applied.

   A transform is generated for each method, therefore each transform
   should only be applied if the most specific method is the
   transform's method. This is done by comparing the type-list for the
   most specific method to the transform's type-list and if they
   match, the transform is applied.

   METHODS is the sorted list of applicable methods.

   TYPE-LIST is the type list for the current transform.

   Return true if the transform should be applied."

  (when methods
    (let ((method (first methods)))
      (equal (lambda-list->type-list (method-lambda-list method)
                                     (mapcar #'specializer->cl (method-specializers method)))
             type-list))))


;;; Utilities

(defun positions (item seq)
  "Return a list containing the positions of the elements of sequence
   SEQ which are EQL to ITEM."

  (loop
     for i from 0
     for elem in seq
     when (eql item elem) collect i))
