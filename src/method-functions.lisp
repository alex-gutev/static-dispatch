;;;; method-functions.lisp
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

;;;; Generation static method functions

(in-package #:static-dispatch)

;;; Declarations

(define-declaration static-dispatch-type (args)
  "Configure how a generic-function is statically dispatched in an environment.

   The arguments are of the form:

     (DISPATCH-TYPE . FUNCTIONS)

   DISPATCH-TYPE is one of the following symbols specifying how the
   generic function is statically dispatched:

     INLINE : The method body is always inlined regardless of the SPACE
              optimization quality level.

     FUNCTION : A call to the ordinary function implementing the
                method is always emitted regardless of the
                optimization quality levels.

     NIL : The default. How the generic function is statically
           dispatched, depends on the SPACE optimization quality
           level.

   FUNCTIONS is a list of generic function names, for which the
   DISPATCH-TYPE applies."

  (destructuring-bind (dispatch-type &rest fns) args
    (check-type dispatch-type (member inline function nil))

    (values
     :function
     (mapcar (rcurry #'list 'dispatch-type dispatch-type) fns))))

(defun static-dispatch-type (fn env)
  "Return the declared STATIC-DISPATCH-TYPE of the function FN, in
   environment ENV."

  (cdr (assoc 'dispatch-type (nth-value 2 (function-information fn env)))))


;;; Generating Method Functions

(defun make-method-function (gf-name name lambda-list body)
  "Generate a DEFUN form implementing a generic function method.

   GF-NAME is the name of the generic function.

   NAME is the name of the function to generate.

   LAMBDA-LIST is the ordinary function's lambda-list.

   BODY is the body implementing the method.

   Returns a DEFUN form for the method function."

  (with-gensyms (call-next-method next-method-p next-arg-var)
    (let ((required (parse-ordinary-lambda-list lambda-list)))
      (multiple-value-bind (forms declarations docstring)
          (parse-body body :documentation t)

        `(defun ,name (,call-next-method ,next-method-p ,@lambda-list)
	   (declare (ignorable ,call-next-method ,next-method-p ,@required))
           ,@declarations
           ,@(ensure-list docstring)

	   (static-method-function-test-hook)

	   (flet ((call-next-method (&rest ,next-arg-var)
		    (apply ,call-next-method ,next-arg-var))

		  (next-method-p ()
		    ,next-method-p))
	     (declare (ignorable #'call-next-method #'next-method-p))

	     (block ,(block-name gf-name)
               ,@forms)))))))
