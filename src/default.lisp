;;;; default.lisp
;;;;
;;;; Copyright 2018-2021 Alexander Gutev
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

;;;; Generic (non-implementation specific) static dispatch.

(in-package #:static-dispatch)


;;; Compiler Macro

(defmacro enable-static-dispatch (&rest names)
  "Enable static dispatching for generic functions with names NAMES."

  (let ((*method-functions* (copy-hash-table *method-functions*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(iter
	   (for name in names)
	   (match name
             ((list :inline name)
              (collect (make-remove-method-function-names name)))

             ((list :function name)
              (collect `(progn ,@(make-static-overload-functions name)))))))))

(defun make-static-dispatch (name lambda-list specializers)
  (declare (ignore name lambda-list specializers))
  nil)

(defun static-dispatch (whole &optional env)
  "Compiler macro function for statically dispatched generic
   functions."

  (or
   (match whole
     ;; Just in case some implementation decides to invoke the
     ;; compiler macro for APPLY and MULTIPLE-VALUE-CALL forms.
     ((cons (or 'apply 'cl:multiple-value-call) _)
      whole)

     ((or
       (list* 'funcall (or (list 'function name)
			   (guard name (symbolp name))) args)
       (cons name args))

      (when (static-dispatch? name env)
	(handler-case
	    (static-overload name args env)

	  (error (e)
            (simple-style-warning "Static dispatch for ~s failed:~%~2T~a" name e)
            whole)))))
   whole))

(defun static-overload (gf-name args env)
  "Determines the types of the generic function (with name GF-NAME)
   arguments ARGS, determines the most applicable method and returns
   the body of the method. If there are no applicable methods, or the
   types of the arguments could not be determined, NIL is returned."

  (unless (and (fboundp gf-name)
               (typep (fdefinition gf-name) 'generic-function))
    (error "~s is not a generic function." gf-name))

  (let* ((*env* env)
	 (gf (fdefinition gf-name))
         (types (mapcar (rcurry #'nth-form-type env) args))
         (methods (compute-applicable-methods% gf-name types)))

    (if methods
        (inline-call gf methods args types (should-check-types? env))
        (error "No applicable methods for argument types: ~s" types))))
