;;;; static-dispatch.lisp
;;;;
;;;; Copyright 2018-2019 Alexander Gutev
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


(defclass method-info ()
  ((body
    :initarg :body
    :accessor body
    :documentation
    "The method function body.")

   (qualifier
    :initarg :qualifier
    :accessor qualifier
    :documentation
    "The method qualifier, NIL for primary methods or :BEFORE, :AFTER,
     :AROUND for auxiliary methods.")

   (lambda-list
    :initarg :lambda-list
    :accessor lambda-list
    :documentation
    "The lambda-list of the method.")

   (specializers
    :initarg :specializers
    :accessor specializers
    :documentation
    "The method's specializers")

   (function-name
    :initarg :function-name
    :accessor function-name
    :documentation
    "Symbol naming a non-generic function which implements the
     method.")

   (remove-on-redefine-p
    :initarg :remove-on-redefine-p
    :accessor remove-on-redefine-p
    :initform nil
    :documentation
    "True if the method should be removed when the DEFGENERIC form is
     re-evaluated."))

  (:documentation
   "Stores the body of a method and the name of a non-generic function
    which contains the method's body."))

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


;;; Global Method Table

(defvar *generic-function-table* (make-hash-table :test #'equal)
  "Hash table mapping generic functions to a list of methods.")

(defun gf-methods (gf-name)
  "Returns the method information for the generic function GF-NAME. A
   hash-table mapping lists of specializers to `METHOD-INFO' objects
   is returned."

  (gethash gf-name *generic-function-table*))

(defun ensure-gf-methods (gf-name)
  "Ensures that a method table for the generic function GF-NAME
   exists, in *GENERIC-FUNCTION-TABLE*."

  (or (ensure-gethash gf-name *generic-function-table* (make-hash-table :test #'equal))
      (error 'not-supported)))

(defun gf-method (gf-name specializers)
  "Returns the `METHOD-INFO', of the method with specializer list
   SPECIALIZERS, of the generic function GF-NAME."

  (aand (gf-methods gf-name) (gethash specializers it)))

(defun (setf gf-method) (value gf-name specializers)
  "Sets the method info, to VALUE, for the method with specializer
   list SPECIALIZERS, of the generic function GF-NAME."

  (setf (gethash specializers (ensure-gf-methods gf-name))
	value))

(defun ensure-method-info (gf-name qualifier specializers &key body lambda-list remove-on-redefine-p)
  "Ensures that the method table, withing *GENERIC-FUNCTION-TABLE*, of
   the generic function GF-NAME contains a method with qualifier
   QUALIFIER, specializers SPECIALIZERS, lambda-list LAMBDA-LIST and
   body BODY. If the table does not contain a method for those
   specializers, a new `METHOD-INFO' object is created."

  (aprog1
      (ensure-gethash
       (list qualifier specializers) (ensure-gf-methods gf-name)

       (make-instance 'method-info
		      :specializers specializers
		      :qualifier qualifier
		      :remove-on-redefine-p remove-on-redefine-p))

    (setf (lambda-list it) lambda-list)
    (setf (body it) body)))

(defun mark-no-dispatch (gf-name)
  "Mark the generic function as one which should not be dispatched
   statically. This is used primarily when unsupported features are
   used in the definition of the generic function or its methods."

  (setf (gethash gf-name *generic-function-table*) nil))


;;; DEFMETHOD macro

(defmacro defmethod (name &rest args)
  (or
   (handler-case
       (multiple-value-bind (qualifier specializers lambda-list body)
	   (parse-method args)

	 (ensure-method-info name qualifier specializers :body body :lambda-list lambda-list)

	 `(progn
	    (eval-when (:compile-toplevel :load-toplevel :execute)
	      (ignore-errors
		(unless (compiler-macro-function ',name)
		  (setf (compiler-macro-function ',name) #'static-dispatch))))

	    ,(alet `(c2mop:defmethod ,name ,@args)
	       (if (has-eql-specializer? specializers)
		   `(aprog1 ,it
		      (ensure-method-info
		       ',name
		       ',qualifier
		       (mapcar #'specializer->cl (method-specializers it))
		       :body ',body
		       :lambda-list ',lambda-list))
		   it))))
     (match-error () (mark-no-dispatch name))
     (not-supported ()))

   `(c2mop:defmethod ,name ,@args)))

(defmacro defgeneric (name (&rest lambda-list) &rest options)
  (handler-case
      (progn
	(let ((methods (ensure-gf-methods name)))
	  (iter
	    (for (key method) in-hashtable methods)
	    (when (remove-on-redefine-p method)
	      (remhash key methods))))

	(mapc
	 (lambda-match
	   ((list* :method args)
	    (multiple-value-bind (qualifier specializers lambda-list body)
		(parse-method args)

	      (ensure-method-info name
				  qualifier
				  specializers
				  :body body
				  :lambda-list lambda-list
				  :remove-on-redefine-p t))))
	 options))
    (match-error () (mark-no-dispatch name))
    (not-supported ()))

  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (ignore-errors
	 (unless (compiler-macro-function ',name)
	   (setf (compiler-macro-function ',name) #'static-dispatch))))

     (c2mop:defgeneric ,name ,lambda-list ,@options)))

(defun parse-method (args)
  (ematch args
    ((or (list* (guard lambda-list (listp lambda-list)) body)
	 (list* (and (or :around :before :after) qualifier)
		(guard lambda-list (listp lambda-list)) body))

     (multiple-value-call #'values
       qualifier
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


;;; Compiler Macro

(defmacro static-dispatch-test-hook ()
  "A form of this macro is inserted in the output of the
   STATIC-DISPATCH compiler macro, in order to allow certain test code
   to be inserted, by shadowing this macro using MACROLET. The
   global expands to NIL."

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
	  (not-supported () whole)))))
   whole))

(defun static-dispatch? (name env)
  "Checks whether the generic function named NAME should be statically
   dispatched. This is the case if it is declared inline in the
   environment ENV."

  (let ((decl (nth-value 2 (function-information name env))))
    (eq (cdr (assoc 'inline decl)) 'inline)))



;;; Method Inlining

(defvar *current-gf* nil
  "The name of the generic function currently being inlined/statically
   dispatched.")

(define-condition illegal-call-next-method-error ()
  ((method-type
    :reader method-type
    :initarg method-type
    :documentation
    "The type of method from which CALL-NEXT-METHOD was called."))

  (:documentation
   "Condition representing an illegal call to CALL-NEXT-METHOD from within a :BEFORE and :AFTER method"))

(cl:defmethod print-object ((e illegal-call-next-method-error) stream)
  (format stream "CALL-NEXT-METHOD called inside ~a method" (method-type e)))


(defun static-overload (gf-name args env)
  "Determines the types of the generic function (with name GF-NAME)
   arguments ARGS, determines the most applicable method and returns
   the body of the method. If there are no applicable methods, or the
   types of the arguments could not be determined, NIL is returned."

  (when (fboundp gf-name)
   (let* ((*current-gf* gf-name)
	  (gf (fdefinition gf-name)))

     (let* ((precedence (precedence-order (generic-function-lambda-list gf) (generic-function-argument-precedence-order gf)))
	    (match-args (order-by-precedence precedence args))
	    (types (get-return-types match-args env))
	    (methods (-<> (aand (gf-methods gf-name) (hash-table-alist it))
			  (order-method-specializers precedence)
			  (applicable-methods types)
			  (sort-methods)
			  (mapcar #'cdr <>))))

       (when methods
	 (let ((gensyms (loop repeat (length args) collect (gensym))))
	   `(let ,(mapcar #'list gensyms args)
	      (declare ,@(mapcar (curry #'list 'type) types gensyms))
	      (static-dispatch-test-hook)
	      ,(inline-methods methods gensyms (should-check-types env) types))))))))


(defun precedence-order (lambda-list precedence)
  "Returns a list of the generic function arguments in argument
   precedence order (PRECEDENCE). Each element in the list is the
   index of the argument within LAMBDA-LIST."

  (mapcar (rcurry #'position lambda-list) precedence))

(defun order-by-precedence (precedence args)
  "Orders the list ARGS by the order specified in PRECEDENCE."

  (mapcar (curry #'elt args) precedence))


(defun order-method-specializers (methods precedence)
  "Orders the specializers of METHODS by the argument precedence order
   PRECEDENCE."

  (flet ((order-specializers (method)
	   (destructuring-bind ((qualifier specializers) . method) method
	     (cons
	      (list
	       qualifier
	       (order-by-precedence precedence specializers))
	      method))))
    (mapcar #'order-specializers methods)))

(defun applicable-methods (methods types)
  "Returns a list of all methods in METHODS which are applicable to
   the types TYPES."

  (labels ((filter-methods (types methods)
	     (when methods
	       (if types
		   (->> (filter-on-type methods (first types))
			(mapcar #'next-specializer)
			(filter-methods (rest types)))
		   methods)))

	   (filter-on-type (methods type)
	     (if (eq type t)
		 (let ((methods (remove 'eql methods :key (compose #'ensure-car #'caar)))) ; Remove all EQL methods
		   (and (every (compose (curry #'eq t) #'caar) methods) methods))
		 (remove type methods :test-not #'subtypep :key #'caar)))

	   (next-specializer (method)
	     (cons (cdar method) (cdr method)))

	   (copy-specializer (method)
	     (cons (cadar method) method)))

    (->> (mapcar #'copy-specializer methods)
	 (filter-methods types)
	 (mapcar #'cdr))))

(defun sort-methods (methods)
  "Sorts METHODS by specificity."

  (sort methods #'method< :key #'car))

(defun method< (s1 s2)
  "Returns true if method with specializers and qualifier S1 should be
   called before S2."

  (destructuring-bind (q1 s1) s1
    (destructuring-bind (q2 s2) s2
      (cond
	((and (eq q1 q2)
	      (eq q1 :after))
	 (specializer< s2 s1))

	((eq q1 q2)
	 (specializer< s1 s2))

	(t
	 (qualifier< q1 q2))))))

(defun qualifier< (q1 q2)
  "Returns true if the method with qualifier Q1 should be called
   before the method with qualifier Q2."

  (match* (q1 q2)
    ((:before (not :before)) t)
    (((not :after) :after) t)
    ((:around (not :around)) t)))

(defun specializer< (s1 s2)
  "Returns true if the specializer list S1 is more specific than
   specializer list S2."

  (labels ((specializer< (s1 s2)
	     (match* (s1 s2)
	       (((list* (list 'eql o1) s1)
		 (list* (list 'eql o2) s2))

		(if (eql o1 o2)
		    (specializer< s1 s2)
		    :equal))

	       (((list* (list 'eql _) _) _)
		:less)

	       ((_ (list* (list 'eql _) _))
		:greater)

	       (((list* type1 s1)
		 (list* type2 s2))

		(let ((class1 (find-class type1))
		      (class2 (find-class type2)))

		  (ensure-finalized class1)
		  (ensure-finalized class2)

		  (let ((prec1 (rest (class-precedence-list class1)))
			(prec2 (rest (class-precedence-list class2))))
		    (cond
		      ((member class2 prec1) :less)
		      ((member class1 prec2) :greater)
		      ((eq class1 class2) (specializer< s1 s2))
		      (t
		       (or
			(when (and s1 s2)
			  (let ((order (specializer< s1 s2)))
			    (unless (eq order :equal)
			      order)))

			(if (string< (class-name class1) (class-name class2))
			    :less
			    :greater))))))))))
    (eq (specializer< s1 s2) :less)))

(defun inline-methods (methods args &optional check-types types)
  "Return a form which contains the body of all the applicable
   methods (METHODS) inline, starting with the most specific method.

   ARGS is either a list of the arguments passed to the methods or a
   symbol naming a variable in which the argument list is stored.

   If CHECK-TYPES is true CHECK-TYPES forms are inserted to check the
   types of the arguments to CALL-NEXT-METHOD.

   TYPES is a list of the argument types, determined from the lexical
   environment."

  (labels
      ((make-before (methods &optional (types types) (args args))
	 (when methods
	   (make-aux-methods
	    :before
	    (first methods) args (rest methods)
	    :check-types check-types
	    :types types)))

       (make-primary (before methods &optional (types types) (args args))
	 (let ((form
		(make-primary-method-form
		 (first methods) args (rest methods)
		 :check-types check-types
		 :types types)))

	   (if before
	       `(progn ,before ,form)
	       form)))

       (make-after (primary methods &optional (types types) (args args))
	 (if methods
	     `(prog1 ,primary
		,(make-aux-methods
		  :after
		  (first methods) args (rest methods)
		  :check-types check-types
		  :types types))

	     primary))

       (make-all (types before primary after &optional (args args))
	 (-> (make-before before types args)
	     (make-primary primary types args)
	     (make-after after types args)))

       (make-around (around before primary after)
	 (if around
	     (make-primary-method-form
	      (first around) args (rest around)
	      :check-types check-types
	      :types types
	      :last-form (curry #'make-all nil before primary after))

	     (make-all types before primary after))))

    (let ((before (remove-if-not (curry #'eql :before) methods :key #'qualifier))
	  (primary (remove-if-not #'null methods :key #'qualifier))
	  (after (remove-if-not (curry #'eql :after) methods :key #'qualifier))
	  (around (remove-if-not (curry #'eql :around) methods :key #'qualifier)))

      (make-around around before primary after))))

(defun make-aux-methods (type method args next-methods &key check-types types)
  "Return a form containing the bodies of auxiliary (:BEFORE and :AFTER) methods inline.

   METHOD is the first method to execute.

   ARGS is the list of arguments or the name of the argument list
   variable.

   NEXT-METHODS is the list of the following methods to be executed.

   CHECK-TYPES is a boolean indicating whether CHECK-TYPES forms
   should be inserted.

   TYPES is the list of the types of the arguments as determined from
   the lexical environment."

  (with-gensyms (next-args)
    `(flet ((call-next-method (&rest ,next-args)
	      (declare (ignore ,next-args))
	      (error 'illegal-call-next-method-error :method-type ,type))

	    (next-method-p () nil))
       (declare (ignorable #'call-next-method #'next-method-p))

       ,@(make-aux-method-body method args next-methods
			       :check-types check-types
			       :types types))))

(defun make-aux-method-body (method args next-methods &key check-types types)
  "Return a list of forms containing the bodies of auxiliary (:BEFORE and :AFTER) methods inline.

   METHOD is the first method to execute.

   ARGS is the list of arguments or the name of the argument list
   variable.

   NEXT-METHODS is the list of the following methods to be executed.

   CHECK-TYPES is a boolean indicating whether CHECK-TYPES forms
   should be inserted.

   TYPES is the list of the types of the arguments as determined from
   the lexical environment."

  (let ((args (if (listp args) (cons 'list args) args)))
    (destructuring-bind (&optional next-method &rest more-methods) next-methods
      (list*
       (make-inline-method-body method args types check-types)
       (when next-method
	 (make-aux-method-body next-method args more-methods
			       :check-types check-types
			       :types types))))))

(defun make-primary-method-form (method args next-methods &key check-types types last-form)
  "Return a form containing the bodies of the primary method inline.

   METHOD is the primary method to be executed.

   ARGS is the list of arguments or the name of the argument list
   variable.

   NEXT-METHODS is the list of the following applicable primary
   methods.

   CHECK-TYPES is a boolean indicating whether CHECK-TYPES forms
   should be inserted.

   TYPES is the list of the types of the arguments as determined from
   the lexical environment.

   LAST-FORM is a function which is called to generate the body of the
   CALL-NEXT-METHOD function of the last applicable method. The
   function is called with one argument, the variable to which the
   argument list is bound. If LAST-FORM is NIL a call to
   NO-NEXT-METHOD is generated in the body of the last
   CALL-NEXT-METHOD function."

  (let ((args (if (listp args) (cons 'list args) args)))
    (destructuring-bind (&optional next-method &rest more-methods) next-methods
      (with-gensyms (next-arg-var next-args)
	`(flet ((call-next-method (&rest ,next-arg-var)
		  (let ((,next-args (or ,next-arg-var ,args)))
		    (declare (ignorable ,next-args))
		    ,(cond
		       (next-method
			(make-primary-method-form
			 next-method next-args more-methods
			 :check-types check-types
			 :last-form last-form))

		       (last-form (funcall last-form next-args))

		       (t
			`(apply #'no-next-method ',*current-gf* nil ,next-args)))))

		(next-method-p ()
		  ,(and (or next-method last-form) t)))
	   (declare (ignorable #'call-next-method #'next-method-p))

	   ,(make-inline-method-body method args types check-types))))))

(defun make-inline-method-body (method args types check-types)
  "Returns the inline method body (without the CALL-NEXT-METHOD and
   NEXT-METHOD-P functions), of a METHOD, when applied on arguments
   ARGS with types TYPES."

  (with-slots (lambda-list specializers body) method
    `(block ,(block-name *current-gf*)
       (destructuring-bind ,lambda-list ,args
	 ,(-> (subseq lambda-list 0 (length specializers))
	      (make-ignorable-declarations))
	 ,@(cond
	     (types
	      (list (make-type-declarations lambda-list types)))
	     (check-types
	      (make-type-checks lambda-list specializers)))
	 ,@(body method)))))


(defun block-name (gf-name)
  "Returns the name of the implicit block, surrounding a method of the
   generic function GF-NAME."

  (match gf-name
    ((list 'setf name)
     name)

    (_ gf-name)))

(defun enclose-in-type-declarations (forms vars types)
  "Encloses FORMS in a LOCALLY form which declares the types of VARS
   to be TYPES."

  `(locally ,(make-type-declarations vars types)
     ,@forms))

(defun make-type-declarations (vars types)
  "Returns a DECLARE expression which declares each variable in VARS
   to be of the type stored in the corresponding element of TYPES"

  `(declare ,@(mapcar (curry #'list 'type) types vars)))

(defun should-check-types (env)
  "Returns true if CHECK-TYPE forms should be added in the body of
   CALL-NEXT-METHOD. CHECK-TYPE forms are added if the priority of
   the SAFETY optimize quality is greater than or equal to the SPEED
   optimize quality in the environment ENV."

  (let ((optimize (declaration-information 'optimize env)))
    (>= (second (assoc 'safety optimize))
	(second (assoc 'speed optimize)))))

(defun make-type-checks (vars types)
  "Returns a list of CHECK-TYPE forms for each variable in VARS and
   corresponding type in TYPES."

  (mapcar (curry #'list 'check-type) vars types))

(defun make-ignorable-declarations (vars)
  "Creates a DECLARE IGNORABLE expression for the variables in VARS."

  `(declare (ignorable ,@vars)))
