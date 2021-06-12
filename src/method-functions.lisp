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

(defun make-static-overload-functions (gf-name)
  "Generate the forms which define the method functions of a generic function.

   Also generates function names for the methods and returns forms
   which set those names in *METHOD-FUNCTIONS*. Thus this function
   should be wrapped in a binding of a copy of *METHOD-FUNCTIONS*, in
   order to prevent it's value from being modified merely by
   macroexpansion.

   GF-NAME is the name of the generic function.

   Returns a list of forms which define the functions that implement
   the methods."

  `((eval-when (:compile-toplevel :load-toplevel :execute)
      ,@(method-function-names gf-name))

    ,@(when-let (methods (gf-methods gf-name))
	(let ((*current-gf* gf-name))
	  (loop
	     for method being the hash-value of methods
	     for fn = (make-method-function gf-name method)
	     when fn collect fn)))))

(defun make-remove-method-function-names (gf-name)
  "Create forms which remove all function names from *METHOD-FUNCTIONS* for a generic function.

   GF-NAME is the generic function name.

   Returns a form which removes all method function names for the
   given generic function."

  (when-let (methods (gf-methods gf-name))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop for spec being the hash-key of methods
	  collect `(remhash ',(cons gf-name spec) *method-functions*)))))

(defun method-function-names (gf-name)
  "Generate function names for each generic function method.

   Modifies *METHOD-FUNCTIONS*.

   GF-NAME is the generic function name.

   Returns a list of forms which store the generated names in
   *METHOD-FUNCTIONS*."

  (when-let (methods (gf-methods gf-name))
    (loop
       for spec being the hash-key of methods
       collect
	 `(setf (gethash '(,gf-name . ,spec) *method-functions*)
		',(method-function-name gf-name spec)))))

(defun method-function-name (gf-name method &optional (generate t))
  "Return the function name for a generic function method.

   GF-NAME is the generic function name.

   METHOD is the method specifier (QUALIFIER SPECIALIZERS).

   GENERATE is a flag which if true, results in a new name being
   generated and stored in *METHOD-FUNCTIONS*.

   Returns the function name."

  (if generate
      (ensure-gethash
       (cons gf-name method) *method-functions*
       (gentemp (symbol-name (block-name gf-name)) *method-function-package*))

      (gethash (cons gf-name method) *method-functions*)))

(defun make-method-function (gf-name method)
  "Generate a DEFUN form implementing a generic function method.

   GF-NAME is the name of the generic function.

   METHOD is the `METHOD-INFO' object containing the method details.

   Returns a DEFUN form for the method."

  (with-slots (lambda-list) method
    (let ((name (method-function-name gf-name (method-spec method))))
      (with-gensyms (call-next-method next-method-p next-arg-var)

	(multiple-value-bind (lambda-list *full-arg-list-form* ignore)
	    (lambda-list->arg-list-form lambda-list)

	  `(defun ,name (,call-next-method ,next-method-p ,@lambda-list)
	     (declare (ignorable ,@ignore))
	     (static-method-function-test-hook)

	     (flet ((call-next-method (&rest ,next-arg-var)
		      (apply ,call-next-method ,next-arg-var))

		    (next-method-p ()
		      ,next-method-p))
	       (declare (ignorable #'call-next-method #'next-method-p))

	       ,(make-inline-method-body method nil nil t))))))))

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
