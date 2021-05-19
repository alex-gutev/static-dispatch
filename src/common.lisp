;;;; common.lisp
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

;;;; Base definitions used on all implementations

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

(defvar *method-function-package* (make-package 'static-dispatch.method-functions)
  "Package into which statically dispatched method function names are
   interned.")

(defvar *method-functions* (make-hash-table :test #'equal)
  "Hash table mapping methods to the function implementing that method.

   Each key is of the form (NAME QUALIFIER SPECIALIZERS) where NAME is
   the name of the generic function, QUALIFIER is the method qualifier
   and SPECIALIZERS is the argument type specializer list.")

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

(defun remove-defined-methods (name)
  "Remove the methods of a generic-function for which REMOVE-ON-REDEFINE-P is true.

   NAME is the name of the generic function."

  (handler-case
      (let ((methods (ensure-gf-methods name)))
	(iter
	  (for (key method) in-hashtable methods)
	  (when (remove-on-redefine-p method)
	    (remhash key methods))))

    (not-supported ())))

(defun method-spec (method-info)
  "Return the method specifier, i.e. the key used within the method table.

   METHOD-INFO is the `METHOD-INFO' object of the method."

  (with-slots (qualifier specializers) method-info
    (list qualifier specializers)))


;;; DEFMETHOD and DEFGENERIC Macros

(defmacro defmethod (name &rest args)
  `(progn
     (walk-environment
       (c2mop:defmethod ,name ,@args))

     ,(handler-case
	  (multiple-value-bind (qualifier specializers lambda-list body)
	      (parse-method args)

	    (declare (ignore lambda-list))

	    (make-add-method-info name qualifier specializers body))

	  (match-error () `(mark-no-dispatch ',name))
	  (not-supported () `(mark-no-dispatch ',name)))))

(defmacro defgeneric (name (&rest lambda-list) &rest options)
  `(progn
     (walk-environment
       (c2mop:defgeneric ,name ,lambda-list ,@options))

     (remove-defined-methods ',name)

     ,@(handler-case
	  (mappend
	   (lambda-match
	     ((list* :method args)
	      (multiple-value-bind (qualifier specializers lambda-list body)
		  (parse-method args)

		(declare (ignore lambda-list))

		(list (make-add-method-info name qualifier specializers body :remove-on-redefine-p t)))))

	   options)

	(match-error () `((mark-no-dispatch ',name))))))

(defun make-add-method-info (name qualifier specializers body &key remove-on-redefine-p)
  "Create a form which adds the method information to the method table.

   NAME is the name of the generic function.

   QUALIFIER is the method qualifier.

   SPECIALIZERS is the method's specializer list as it appears in the
   DEFMETHOD form.

   REMOVE-ON-REDEFINE-P is a flag for whether the method should be
   removed when the generic function is redefined."

  (flet (#-clisp
	 (make-specializer (specializer)
	   (match specializer
	     ((list 'eql value)
	      ``(eql ,,value))

	     (_ `',specializer)))

	 #+clisp
	 (make-specializer (specializer)
	   (match specializer
	     ((list 'eql value)
	      `(intern-eql-specializer ,value))

	     (_
	      `(find-class ',specializer nil)))))

    (with-gensyms (method)
      `(let ((,method (find-method
		       (fdefinition ',name)
		       ',(ensure-list qualifier)
		       (list ,@(mapcar #'make-specializer specializers))
		       nil)))
	 (when ,method
	   (ensure-method-info
	    ',name
	    ',qualifier
	    (mapcar #'specializer->cl (method-specializers ,method))
	    :body ',body
	    :lambda-list (method-lambda-list ,method)
	    :remove-on-redefine-p ,remove-on-redefine-p))))))


;;; Parsing Method Definitions

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


;;; Static Dispatching

(defmacro static-dispatch-test-hook ()
  "A form of this macro is inserted in the output of the
   STATIC-DISPATCH compiler macro, in order to allow certain test code
   to be inserted, by shadowing this macro using MACROLET. The
   global expands to NIL."

  nil)

(defmacro static-method-function-test-hook ()
  "A form of this macro is inserted in method functions, in order to
   allow test code to be inserted."

  nil)

(defun static-dispatch? (name env)
  "Checks whether the generic function named NAME should be statically
   dispatched. This is the case if it is declared inline in the
   environment ENV."

  (let ((decl (nth-value 2 (function-information name env))))
    (eq (cdr (assoc 'inline decl)) 'inline)))


;;; Method Inlining

(defvar *env* nil
  "Environment in which the current generic function is being
   statically dispatched.")

(defvar *current-gf* nil
  "The name of the generic function currently being inlined/statically
   dispatched.")

(defvar *full-arg-list-form* nil
  "Bound to a from which constructs the full argument list for use as
   the default argument list in CALL-NEXT-METHOD.")

(define-condition illegal-call-next-method-error (error)
  ((method-type
    :reader method-type
    :initarg :method-type
    :documentation
    "The type of method from which CALL-NEXT-METHOD was called."))

  (:documentation
   "Condition representing an illegal call to CALL-NEXT-METHOD from within a :BEFORE and :AFTER method"))

(cl:defmethod print-object ((e illegal-call-next-method-error) stream)
  (format stream "CALL-NEXT-METHOD called inside ~a method" (method-type e)))

(define-condition no-primary-method-error (error)
  ((gf-name
    :reader gf-name
    :initarg :gf-name
    :documentation
    "Generic function name.")

   (args
    :reader args
    :initarg :args
    :documentation
    "Method arguments."))

  (:documentation
   "No primary method for a generic function with given arguments."))

(cl:defmethod print-object ((e no-primary-method-error) stream)
  (format stream "No primary method for generic function ~a with arguments ~a"
	  (gf-name e) (args e)))

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

(defun methods-for-types (name types &optional (remove-t t))
  "Return the list of applicable methods for a given set of types.

   NAME is the generic function name.

   TYPES is the a list of type specifiers for the required arguments.

   REMOVE-T is a flag, which if true, results in T methods being
   removed if they match a T specifier.

   Returns the list of applicable methods, `METHOD-INFO' objects, in
   descending order of priority, with the method which should be
   called first ordered first."

  (let* ((gf (fdefinition name))
	 (precedence (precedence-order (generic-function-lambda-list gf) (generic-function-argument-precedence-order gf)))
	 (types (order-by-precedence precedence types)))
    (-<> (aand (gf-methods name) (hash-table-alist it))
	 (order-method-specializers precedence)
	 (applicable-methods types remove-t)
	 (sort-methods)
	 (mapcar #'cdr <>))))

(defun applicable-methods (methods types &optional (remove-t t))
  "Returns a list of all methods in METHODS which are applicable to
   the types TYPES.

   If REMOVE-T is true, applicable methods with a T specializer which
   match a T argument type are not considered unless all other methods
   have a T specializer for that argument. If NIL they are considered
   for applicability."

  (labels ((filter-methods (types methods)
	     (when methods
	       (if types
		   (->> (filter-on-type methods (first types))
			(mapcar #'next-specializer)
			(filter-methods (rest types)))
		   methods)))

	   (filter-on-type (methods type)
	     (if (and remove-t (eq type t))
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

  (multiple-value-bind (bindings args declarations)
      (make-argument-bindings args types)

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
		  (primary-method-form methods types args)))

	     (if before
		 `(progn ,before ,form)
		 form)))

	 (primary-method-form (methods types args)
	   (match methods
	     ((list* first rest)
	      (make-primary-method-form
	       first args rest
	       :check-types check-types
	       :types types))

	     (_
	      `(error 'no-primary-method-error
		      :gf-name ',*current-gf*
		      :args ,(if (listp args) (cons 'list args) args)))))

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

	`(let ,bindings
	   ,@(when declarations
	       `((declare ,@declarations)))

	   ,(make-around around before primary after))))))

(defun make-argument-bindings (args types)
  "Generating LET bindings which bind the arguments to variables.

   ARGS is the list of argument forms passed to the generic function
   call. If it's a symbol it represents the name of a variable which
   stores the argument list.

   TYPES is the list of type specifiers of the arguments, as
   determined from the environment.

   Returns three values:

    1. A list of LET bindings.
    2. The new argument list.
    3. A list of declarations relating to the variables."

  (etypecase args
    (symbol (values nil args nil))

    (list
     (iter
       (for i from 0)
       (for arg in args)
       (for (type . rest-types) initially types then rest-types)

       (cond
	 ((constantp arg *env*)
	  (collect arg into arg-list))

	 (t
	  (let ((var (gensym (format nil "a~a" i))))
	    (collect (list var arg) into bindings)
	    (collect var into arg-list)

	    (when type
	      (collect `(type ,type ,var) into declarations)))))

       (finally (return (values bindings arg-list declarations)))))))

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

  (destructuring-bind (&optional next-method &rest more-methods) next-methods
    (list*
     (make-inline-method-body method args types check-types)
     (when next-method
       (make-aux-method-body next-method args more-methods
			     :check-types check-types
			     :types types)))))

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

  (destructuring-bind (&optional next-method &rest more-methods) next-methods
    (aif (method-function-name *current-gf* (method-spec method) nil)
	 (make-method-function-call it args)

	 (with-gensyms (next-arg-var next-args)
	   `(flet ((call-next-method (&rest ,next-arg-var)
		     (let ((,next-args (or ,next-arg-var ,(next-method-default-args args))))
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
       ,(destructure-args
	 args
	 lambda-list

	 (multiple-value-bind (forms declarations)
	     (parse-body body :documentation t)

	   `(,@(when args
		 (-> (subseq lambda-list 0 (length specializers))
		     (make-ignorable-declarations)
		     list))

	       ,@(when types
		   (list (make-type-declarations lambda-list types)))

	       ,@declarations

	       ,@(when (and (null types) check-types)
		   (make-type-checks lambda-list specializers))

	       ,@forms))))))

(defun make-method-function-call (function args)
  "Generate call to the function which implements a method.

   FUNCTION is the name of the function which implements the method.

   ARGS is the argument specifier of the method. Either a list
   containing the argument forms, or a symbol naming the variable in
   which the argument list is stored.

   Returns the function call form."

  (etypecase args
    (list
     (cons function args))

    (symbol
     `(apply #',function ,args))))

(defun destructure-args (args lambda-list body)
  "Destructure the argument list, based on the lambda-list, if possible.

   Generates a binding form for the variables in a lambda-list which
   are bound to the argument forms of the arguments to the generic
   function call.

   ARGS is the list of argument forms passed to the generic function.

   LAMBDA-LIST is the method lambda list.

   BODY is the list of forms comprising the method body which are
   inserted in the body of the binding form. The first element of BODY
   may be a declare expression.

   Returns a binding form, binding all variables in LAMBDA-LIST and
   containing BODY."

  (etypecase args
    (null
     `(let () ,@body))

    (cons
     (handler-case
	 `(let* ,(destructure-list lambda-list args)
	    ,@body)

       (error ()
	 `(destructuring-bind ,lambda-list (list ,@args)
	    ,@body))))

    (symbol
     `(destructuring-bind ,lambda-list ,args
	,@body))))

(defun destructure-list (lambda-list list)
  "Destructure a list.

   LAMBDA-LIST is the lambda-list specifying how LIST is destructured.

   LIST is the list of forms which are destructured.

   Returns a list of LET* bindings which bind the variables in
   LAMBDA-LIST to the corresponding forms in LIST. If destructuring
   fails an error condition is signalled."

  (labels ((optional-vars (optional)
	     ;; Extract variable names from optional argument
	     ;; specifiers.

	     (mappend #'optional-var optional))

	   (optional-var (spec)
	     ;; Extract variable names from a single optional argument
	     ;; specifier.

	     (ematch spec
	       ((list name _ nil)
		(list name))

	       ((list name _ sp)
		(list name sp))))

	   (key-vars (key)
	     ;; Extract variable names from keyword argument
	     ;; specifiers.

	     (mappend #'key-var key))

	   (key-var (spec)
	     ;; Extract variable names from a single keyword argument
	     ;; specifier.

	     (ematch spec
	       ((list (list _ name) _ nil)
		(list name))

	       ((list (list _ name) _ sp)
		(list name sp))))

	   (aux-vars (aux)
	     ;; Extract variable names from auxiliary argument
	     ;; specifiers.

	     (mapcar #'first aux))

	   (quote-init-form (spec)
	     ;; Quote the initialization form in an argument
	     ;; specifier. If the argument does not have an
	     ;; initialization form, returns it as is.

	     (match spec
	       ((list* name init sp)
		(list* name `',init sp))

	       (_ spec)))

	   (constant-keywords-p (args)
	     ;; Check that each of the keywords in the keyword portion
	     ;; are constant expressions.
	     (loop for arg in args by #'cddr
		  always (constantp arg *env*))))

    (multiple-value-bind (required optional rest key allow-other-keys aux)
	(parse-ordinary-lambda-list lambda-list)

      (when (and allow-other-keys
		 (->> (+ (length required) (length optional))
		      (subseq list)
		      constant-keywords-p
		      not))
	(error "Compile-time destructuring failed: non-constant keywords."))

      (let ((vars (append required
			  (optional-vars optional)
			  (ensure-list rest)
			  (key-vars key)
			  (aux-vars aux))))

	(eval
	 `(destructuring-bind ,(mapcar #'quote-init-form lambda-list)
	      (list ,@(mapcar (curry #'list 'quote) list))

	    (list
	     ,@(loop
		  for var in vars
		  collect
		    (if (eq var rest)
			`(list ',var `(list ,@,var))
			`(list ',var ,var))))))))))

(defun next-method-default-args (args)
  "Generate the default CALL-NEXT-METHOD argument list form.

   ARGS is the argument list specifier of the arguments passed to the
   current method. This may either be a list of forms or a symbol
   which names a variable in which the full list is stored.

   Returns a form that creates the default CALL-NEXT-METHOD argument
   list."

  (etypecase args
    (null
     *full-arg-list-form*)

    (cons
     `(list ,@args))

    (symbol
     args)))

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
    (or (eql (second (assoc 'safety optimize)) 0)
	(eql (second (assoc 'speed optimize)) 3))))

(defun make-type-checks (vars types)
  "Returns a list of CHECK-TYPE forms for each variable in VARS and
   corresponding type in TYPES."

  (mapcar (curry #'list 'check-type) vars types))

(defun make-ignorable-declarations (vars)
  "Creates a DECLARE IGNORABLE expression for the variables in VARS."

  `(declare (ignorable ,@vars)))


;;; Generating Method Functions

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
	     when (null (qualifier method))
	     collect (make-method-function gf-name method))))))

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
       for (qualifier) = spec
       when (null qualifier)
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

  (with-slots (specializers lambda-list) method
    (let ((methods (methods-for-types gf-name specializers nil)))
      (assert methods (methods) "No applicable methods for specializers: ~a" specializers)

      (multiple-value-bind (lambda-list *full-arg-list-form* ignore)
	  (lambda-list->arg-list-form lambda-list)

	(let ((*method-functions* (copy-hash-table *method-functions*))
	      (name (method-function-name gf-name (method-spec method))))

	  (remhash (cons gf-name (method-spec method)) *method-functions*)

	  `(defun ,name ,lambda-list
	     (declare (ignorable ,@ignore))
	     (static-method-function-test-hook)
	     ,(inline-methods methods nil t)))))))

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

(defun unparse-lambda-list (required optional rest key allow-other-keys aux keyp)
  "Construct a lambda-list out of its components.

   REQUIRED is the list of required arguments.

   OPTIONAL is the list of optional argument specifiers.

   REST is the rest argument, or NIL if there is no rest argument.

   ALLOW-OTHER-KEYS is a flag for whether &ALLOW-OTHER-KEYS should be
   present.

   AUX is the list of auxiliary variable specifiers.

   KEYP is a flag for whether &key should be present.

   Returns the lambda list."

  (append
   required
   (when optional (list* '&optional optional))
   (when rest (list '&rest rest))
   (when keyp '(&key))
   key
   (when allow-other-keys '(&allow-other-keys))
   (when aux (list* '&aux aux))))
