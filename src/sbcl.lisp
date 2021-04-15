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

(defun set-method-compiler-macro (name qualifier specializers lambda-list body)
  (declare (ignore qualifier lambda-list body))
  (with-gensyms (args)
    `(progn
       ;; Make function known to SBCL. Wrap in handler-case to handle
       ;; error when function is already known
       (handler-case (sb-c:defknown ,name * *)
	 (error ()))

       (sb-c:deftransform ,name ((&rest ,args) (,@specializers &rest *) * :policy (> speed safety))
	 ;; TODO: The :NODE argument can be used to obtain the
	 ;; compilation node from which the lexical environment of the
	 ;; function call can be obtained using (SB-C::NODE-LEXENV
	 ;; NODE), which can be used to base the inlining logic on
	 ;; whether there is an inline declaration, in force.
	 ;;
	 ;; Unfortunately the SBCL node functions are not exported
	 ;; from the package and are highly likely to be changed in
	 ;; the future which will break not only this library but all
	 ;; code which uses it.

	 (or (static-overload ',name ',args ',specializers)
	     (sb-c::give-up-ir1-transform))))))

(defun set-defgeneric-compiler-macro (name methods)
  (mapcar (curry #'apply #'set-method-compiler-macro name) methods))


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
