;;;; static-dispatch.lisp
;;;;
;;;; Copyright 2018 Alexander Gutev
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

(in-package #:static-dispatch)


(defclass method-body ()
  ((body
    :initarg :body
    :accessor body
    :documentation
    "The method function body.")

   (function-name
    :initarg :function-name
    :initform nil
    :accessor function-name
    :documentation
    "Symbol naming a non-generic function which implements the
     method."))

  (:documentation
   "Stores the body of a method and the name of a non-generic function
    which contains the method's body."))


(defvar *generic-function-table* (make-hash-table :test #'eq)
  "Hash table mapping generic functions to a list of methods.")

(defun gf-methods (gf-name)
  "Returns the method information for the generic function GF-NAME. A
   hash-table mapping lists of specializers to `METHOD-BODY' objects
   is returned."

  (gethash gf-name *generic-function-table*))

(defun gf-method (gf-name specializers)
  "Returns the `METHOD-BODY', of the method with specializer list
   SPECIALIZERS, of the generic function GF-NAME."

  (aand (gf-methods gf-name) (gethash specializers it)))

(defun ensure-method-info (gf-name specializers &optional body)
  "Ensures that the method table, withing *GENERIC-FUNCTION-TABLE*, of
   the generic function GF-NAME contains a method with specializers
   SPECIALIZERS and body BODY. If the table does not contain a method
   for those specializers, a new `METHOD-BODY' object is created."

  (-<> (ensure-gethash gf-name *generic-function-table* (make-hash-table :test #'equal))
       (ensure-gethash specializers <>
		       (make-instance 'method-body
				      :function-name (gensym (symbol-name gf-name))
				      :body body))))


;;;; DEFMETHOD macro

(defmacro defmethod (name &rest args)
  (multiple-value-bind (specializers lambda-list body) (parse-method args)
    (with-slots (function-name) (ensure-method-info name specializers body)
      `(progn
	 (c2mop:defmethod ,name ,@args)

	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (setf (compiler-macro-function ',name) #'gf-compiler-macro))

	 (defun ,function-name ,lambda-list ,@body)))))

(defun parse-method (args)
  (match args
    ((list* (guard lambda-list #'listp) body)
     (multiple-value-call #'values
       (parse-method-lambda-list lambda-list)
       body))))

(defun parse-method-lambda-list (lambda-list)
  (iter
    (for (x . rest) on lambda-list)
    (match x
      ((guard x (member x lambda-list-keywords))
       (return (values specializers (append required (cons x rest)))))

      ((or (list x specializer)
	   x)
       (collect x into required)
       (collect (or specializer t) into specializers)))

    (finally (return (values specializers required)))))


;;;; Compiler Macro

(define-declaration dispatch (decl)
  (destructuring-bind (type &rest fns) decl
    (values
     :function
     (mapcar (rcurry #'list 'dispatch type) fns))))


(defun gf-compiler-macro (whole &optional env)
  "Compiler macro function for statically dispatched generic
   functions."

  (or
   (match whole
     ((cons name args)
      (when (should-overload? name env)
	(static-overload name args))))
   whole))

(defun should-overload? (name env)
  "Returns true if the generic function NAME should be statically
   dispatched. A generic function should be statically dispatched when
   its DISPATCH-TYPE is STATIC, in the environment ENV."

  (eq 'static (cdr (assoc 'dispatch (nth-value 2 (function-information name env))))))

(defun static-overload (gf-name args)
  "Returns a MATCH form which matches the arguments ARGS to the method
   specializers, of the generic function GF-NAME, and calls the most
   specific method function."

  (let* ((gf (fdefinition gf-name)))

    (with-accessors
	  ((lambda-list generic-function-lambda-list)
	   (precedence generic-function-argument-precedence-order)
	   (methods generic-function-methods))
	gf

      (let* ((precedence (precedence-order lambda-list precedence))
	     (gensyms (repeat-function #'gensym (length args)))
	     (match-gensyms (order-by-precedence precedence gensyms))
	     (methods (sort-methods (order-method-specializers precedence methods))))

	(labels ((make-clause (method)
		   (destructuring-bind (method . specializers) method
		     (list
		      (mapcar #'specializer-pattern specializers)
		      `(locally (declare ,@(mapcar (curry #'list 'type) method gensyms))
			 (,(function-name (gf-method gf-name method)) ,@gensyms)))))

		 (specializer-pattern (specializer)
		   (list 'type (class-name specializer))))


	  `(let ,(mapcar #'list gensyms args)
	     (match* ,match-gensyms
	       ,@(mapcar #'make-clause methods))))))))


(defun precedence-order (lambda-list precedence)
  "Returns a list of the generic function arguments in argument
   precedence order (PRECEDENCE). Each element in the list is the
   index of the argument within LAMBDA-LIST."

  (mapcar (rcurry #'position lambda-list) precedence))

(defun order-by-precedence (precedence args)
  "Orders the list ARGS by the order specified in PRECEDENCE."

  (mapcar (curry #'elt args) precedence))

(defun order-method-specializers (precedence methods)
  "Orders the specializers of each method in METHODS, by the order
   specified in PRECEDENCE. Returns an association list where each key
   is the original specializer list of a method, converted to CL
   notation, and the corresponding value is the method's specializer
   list in argument precedence order."

  (iter (for method in methods)
	(with-accessors ((specializers method-specializers)) method
	  (collect
	      (cons (mapcar #'specializer->cl specializers)
		    (order-by-precedence precedence specializers))))))

(defun specializer->cl (specializer)
  "Returns the CL representation of a specializer as used in a
   DEFMETHOD lambda-list. `CLASS' specializers are replaced with their
   CLASS-NAME and EQL specializers are replaced with `(EQL
   ,EQL-SPECIALIZER-OBJECT). The EQL-SPECIALIZER-OBJECT is the value
   to which the EQL object form was evaluated not the form itself."

  (match specializer
    ((class class)
     (class-name specializer))

    ((class eql-specializer)
     `(eql ',(eql-specializer-object specializer)))))


(defun sort-methods (methods)
  "Sorts METHODS in order of applicability based on their
   specializers."

  (sort methods #'specializer< :key #'cdr))

(defun specializer< (s1 s2)
  "Specializer comparison function. Returns true if the specializer
   list S1 should be ordered after S2."

  (match* (s1 s2)
    (((list* class1 s1)
      (list* class2 s2))

     (let ((prec1 (class-precedence-list class1))
	   (prec2 (class-precedence-list class2)))
       (cond
	 ((member class2 prec1) t)
	 ((member class1 prec2) nil)
	 ((eq class1 class2) (specializer< s1 s2)))))

    ((_ _) t)))
