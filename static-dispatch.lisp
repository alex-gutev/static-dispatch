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

(defun ensure-gf-methods (gf-name)
  "Ensures that a method table for the generic function GF-NAME
   exists, in *GENERIC-FUNCTION-TABLE*."

  (ensure-gethash gf-name *generic-function-table* (make-hash-table :test #'equal)))

(defun gf-method (gf-name specializers)
  "Returns the `METHOD-BODY', of the method with specializer list
   SPECIALIZERS, of the generic function GF-NAME."

  (aand (gf-methods gf-name) (gethash specializers it)))

(defun (setf gf-method) (value gf-name specializers)
  "Sets the method info, to VALUE, for the method with specializer
   list SPECIALIZERS, of the generic function GF-NAME."

  (setf (gethash specializers (ensure-gf-methods gf-name))
	value))

(defun ensure-method-info (gf-name specializers &optional body)
  "Ensures that the method table, withing *GENERIC-FUNCTION-TABLE*, of
   the generic function GF-NAME contains a method with specializers
   SPECIALIZERS and body BODY. If the table does not contain a method
   for those specializers, a new `METHOD-BODY' object is created."

  (aprog1
      (ensure-gethash specializers (ensure-gf-methods gf-name)
		      (make-instance 'method-body
				     :function-name (gensym (symbol-name gf-name))))
    (setf (body it) body)))


;;;; DEFMETHOD macro

(defmacro defmethod (name &rest args)
  (handler-case
      (multiple-value-bind (specializers lambda-list body) (parse-method args)
	(let ((fn-name (function-name (ensure-method-info name specializers body))))
	  `(progn
	     ,(alet `(c2mop:defmethod ,name ,@args)
		    (if (has-eql-specializer? specializers)
			`(aprog1 ,it
			   (setf (gf-method ',name (mapcar #'specializer->cl (method-specializers it)))
				 (make-instance 'method-body :function-name ',fn-name :body ',body)))
			it))

	     (eval-when (:compile-toplevel :load-toplevel :execute)
	       (setf (compiler-macro-function ',name) #'gf-compiler-macro))

	     (defun ,fn-name ,lambda-list ,@body))))
    (match-error () `(cl:defmethod ,name ,@args))))

(defun parse-method (args)
  (ematch args
    ((list* (guard lambda-list (listp lambda-list)) body)
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

(defun has-eql-specializer? (specializers)
  "Returns true if SPECIALIZERS contains EQL
   specializers. SPECIALIZERS should be the list of specializers
   extracted from a DEFMETHOD lambda list."

  (some (lambda-match ((list 'eql _) t)) specializers))


;;;; Compiler Macro

(define-declaration dispatch (decl)
  (destructuring-bind (type &rest fns) decl
    (values
     :function
     (mapcar (rcurry #'list 'dispatch type) fns))))


(define-condition not-supported (error)
  ((feature
    :initarg :feature
    :initform nil
    :reader feature
    :documentation
    "Symbol identifying the unsupported feature."))

  (:documentation
   "Error condition: A CLOS feature was used that is not supporting in
    inlining/static dispatch."))

(defun gf-compiler-macro (whole &optional env)
  "Compiler macro function for statically dispatched generic
   functions."

  (or
   (match whole
     ((cons name args)
      (awhen (static-dispatch? name env)
	(handler-case
	    (static-overload name args it)
	  (not-supported () whole)))))
   whole))

(defun static-dispatch? (name env)
  "Returns a symbol indicating the type of optimization which should
   be performed by the compiler macro for the generic function
   NAME. OVERLOAD is returned if the generic function should be
   dispatched statically, INLINE if the body of the methods should be
   additionally inlined, NIL if no optimizations should be performed."

  (let ((decl (nth-value 2 (function-information name env))))
    (or (and (eq (cdr (assoc 'inline decl)) 'inline) 'inline)
	(and (eq (cdr (assoc 'dispatch decl)) 'static) 'overload))))

(defun static-overload (gf-name args dispatch-type)
  "Returns a MATCH form which matches the arguments ARGS to the method
   specializers, of the generic function GF-NAME, and calls the most
   specific method function. If TYPE is OVERLOAD the bodies of the
   match clauses call the method functions, otherwise if TYPE is
   INLINE the bodies of the method functions are inserted directly
   into the match clauses."

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
		     (let ((lambda-list (method-lambda-list method))
			   (cl-specializers (mapcar #'specializer->cl (method-specializers method))))
		       (list
			(mapcar #'specializer-pattern specializers)
			(make-match-body lambda-list cl-specializers)))))

		 (make-match-body (lambda-list specializers)
		   (ecase dispatch-type
		     (overload
		      `(locally (declare ,@(mapcar (curry #'list 'type) specializers gensyms))
			 (,(function-name (gf-method gf-name specializers)) ,@gensyms)))

		     (inline
		      `(destructuring-bind ,lambda-list (list ,@gensyms)
			 (declare ,@(mapcar (curry #'list 'type) specializers lambda-list))
			 ,@(body (gf-method gf-name specializers))))))

		 (specializer-pattern (specializer)
		   (match (specializer->cl specializer)
		     ((list 'eql object)
		      `(eql ',object))

		     (type `(type ,type)))))


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
   is the method and the corresponding value is the method's
   specializer list in argument precedence order."

  (iter (for method in methods)
	(when (method-qualifiers method)
	  (error 'not-supported :feature 'method-qualifiers))

	(collect
	    (cons method
		  (order-by-precedence precedence (method-specializers method))))))

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
     `(eql ,(eql-specializer-object specializer)))))


(defun sort-methods (methods)
  "Sorts METHODS in order of applicability based on their
   specializers."

  (sort methods #'specializer< :key #'cdr))

(defun specializer< (s1 s2)
  "Specializer comparison function. Returns true if the specializer
   list S1 should be ordered before S2."

  (match* (s1 s2)
    (((list* (eql-specializer) _) _)
     t)

    ((_ (list* (eql-specializer) _))
     nil)

    (((list* class1 s1)
      (list* class2 s2))

     (let ((prec1 (class-precedence-list class1))
	   (prec2 (class-precedence-list class2)))
       (cond
	 ((member class2 prec1) t)
	 ((member class1 prec2) nil)
	 ((eq class1 class2) (specializer< s1 s2)))))))
