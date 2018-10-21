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

(in-readtable all-lambda-syntax)


(defclass method-body ()
  ((body
    :initarg :body
    :accessor body
    :documentation
    "The method function body.")

   (lambda-list
    :initarg :lambda-list
    :accessor lambda-list
    :documentation
    "The methods lambda-list, as a `LAMBDA-LIST' object.")

   (function-name
    :initarg :function-name
    :initform nil
    :accessor function-name
    :documentation
    "Symbol naming a non-generic function which implements the
     method. This function has the same names")))


(defvar *generic-function-table* (make-hash-table :test #'eq)
  "Hash table mapping generic functions to a list of methods.")

(defun gf-methods (gf-name)
  (gethash gf-name *generic-function-table*))

(defun gf-method (gf-name specializers)
  (aand (gf-methods gf-name) (gethash specializers it)))

(defun ensure-method-info (gf-name lambda-list &optional body)
  (with-slots (specializers) lambda-list
    (-<> (ensure-gethash gf-name *generic-function-table* (make-hash-table :test #'equal))
	 (ensure-gethash specializers <>
			 (make-instance 'method-body
					:function-name (gensym (symbol-name gf-name))
					:lambda-list lambda-list
					:body body)))))


(defmacro defmethod (name &rest args)
  (multiple-value-bind (lambda-list body) (parse-method args)
    (with-slots (function-name) (ensure-method-info name lambda-list body)
      `(progn
	 (c2mop:defmethod ,name ,@args)

	 (eval-when (:compile-toplevel :load-toplevel :execute)
	   (setf (compiler-macro-function ',name) #'gf-compiler-macro))

	 (defun ,function-name ,(create-lambda-list lambda-list)
	   ,@body)))))

(defun parse-method (args)
  (match args
    ((list* (guard lambda-list #'listp) body)
     (values (parse-generic-lambda-list lambda-list) body))))


;;; Compiler Macro

(define-declaration dispatch (decl)
  (destructuring-bind (type &rest fns) decl
    (values
     :function
     (mapcar #`(,a1 dispatch ,type) fns))))


(defun gf-compiler-macro (whole &optional env)
  (or
   (match whole
     ((cons name args)
      (when (should-overload? name env)
	(static-overload name args env))))
   whole))

(defun should-overload? (name env)
  (eq 'static (cdr (assoc 'dispatch (nth-value 2 (function-information name env))))))

(defun static-overload (gf-name args env)
  (let* ((gf (fdefinition gf-name))
	 (types (get-types args env)))

    (with-accessors
	  ((lambda-list generic-function-lambda-list)
	   (precedence generic-function-argument-precedence-order)
	   (methods generic-function-methods))
	gf

      (awhen (find-methods lambda-list precedence types methods)
	(when-let ((method (gf-method gf-name (mapcar #'class-name (first it)))))
	  `(,(function-name method) ,@args))))))


(defun get-types (args env)
  (mapcar (rcurry #'get-type env) args))

(defun get-type (thing env)
  (or
   (match thing
     ((type symbol)
      (cdr (assoc 'type (nth-value 2 (variable-information thing env)))))

     ;; TODO: Expand macros and check for constant values
     ((list* 'the type _)
      type))

   t))


(defun find-methods (lambda-list precedence types methods)
  (flet ((method-specializers (method)
	   (with-accessors ((specializers method-specializers)) method
	     (cons specializers (order-by-precedence lambda-list precedence specializers)))))
    (let ((types (order-by-precedence lambda-list precedence types))
	  (methods (mapcar #'method-specializers methods)))

      ;;; Sort methods by precedence
      (sort-methods (match-methods methods types) lambda-list precedence))))

(defun match-methods (methods types)
  ;; Ignore EQL specializers for now
  (flet ((applicable? (type specializer)
	   (match specializer
	     ((class class)
	      (subtypep type (class-name specializer)))))

	 (is-t? (specializer)
	   (match specializer
	     ((class class)
	      (eq (class-name specializer) t)))))

    (match types
      ((list* 't rest)
       (when (every (compose #'is-t? #'cadr) methods)
	 (match-methods
	  (mapcar #L(cons (car %1) (cdr %1)) methods)
	  rest)))

      ((list* type rest)
       (match-methods
	(mapcar #L(cons (car %1) (cddr %1))
		(remove type methods :test-not #'applicable? :key #'cadr))
	rest))

      (nil (mapcar #'car methods)))))

(defun sort-methods (methods lambda-list precedence)
  (let ((methods (mapcar #L(cons %1 (order-by-precedence lambda-list precedence %1)) methods)))
    (mapcar #'car (sort methods #'specializer< :key #'cdr))))

(defun specializer< (s1 s2)
  (multiple-value-match (values s1 s2)
    (((list* class1 s1)
      (list* class2 s2))

     (let ((prec1 (class-precedence-list class1))
	   (prec2 (class-precedence-list class2)))
       (cond
	 ((member class2 prec1) t)
	 ((member class1 prec2) nil)
	 ((eq class1 class2) (specializer< s1 s2)))))

    ((_ _) t)))

(defun order-by-precedence (lambda-list precedence args)
  (let ((args (mapcar #'cons lambda-list args)))
    (mapcar (compose #'cdr (rcurry #'assoc args)) precedence)))
