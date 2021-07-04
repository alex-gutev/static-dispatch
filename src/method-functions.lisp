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
    (multiple-value-bind (lambda-list *full-arg-list-form* ignore)
	(lambda-list->arg-list-form lambda-list)

      (multiple-value-bind (forms declarations docstring)
          (parse-body body :documentation t)

        `(defun ,name (,call-next-method ,next-method-p ,@lambda-list)
	   (declare (ignorable ,call-next-method ,next-method-p ,@ignore))
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

(defun lambda-list->arg-list-form (lambda-list)
  "Construct a form which recreates an argument list from a lambda-list.

   LAMBDA-LIST is an ordinary lambda-list

   Returns three values:

     1. The new lambda-list with supplied-p variables added where
        necessary.

     2. The form which recreates the argument list.

     3. A list of variables which should be declared IGNORABLE."

  (let (sp-vars)
    (labels ((add-sp (spec)
	       (ematch spec
		 ((list var init nil)
		  (let ((sp (gensym)))
		    (push sp sp-vars)
		    (list var init sp)))

		 ((list var init sp)
		  (list var init sp)))))

      (multiple-value-bind (required optional rest key allow-other-keys aux keyp)
	  (parse-ordinary-lambda-list lambda-list)

	(let* ((optional (mapcar #'add-sp optional))
	       (rest (or rest (if allow-other-keys (gensym "REST"))))
	       (key (mapcar #'add-sp key))
	       (lambda-list
		(unparse-lambda-list required optional rest key allow-other-keys aux keyp)))

	  (labels ((make-required (required)
		     (ematch required
		       ((list* var rest)
			`(cons ,var ,(make-required rest)))

		       (nil
			(make-optional optional))))

		   (make-optional (optional)
		     (ematch optional
		       ((list* (list var _ sp) rest)
			`(if ,sp (cons ,var ,(make-optional rest))))

		       (nil
			(make-rest rest))))

		   (make-rest (rest)
		     (or rest (make-key key)))

		   (make-key (key)
		     (ematch key
		       ((list* (list (list keyword var) _ sp) rest)
			`(if ,sp (list* ',keyword ,var ,(make-key rest))))

		       (nil nil))))

	    (values
	     lambda-list
	     (make-required required)
	     sp-vars)))))))
