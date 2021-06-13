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

   (qualifiers
    :initarg :qualifiers
    :accessor qualifiers
    :documentation
    "List of method qualifiers.")

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


;;; Global Method Table

(defvar *generic-function-table* (make-hash-table :test #'equal)
  "Hash table mapping generic functions to a list of methods.")

(defvar *method-function-package* (make-package 'static-dispatch.method-functions)
  "Package into which statically dispatched method function names are
   interned.")

(defvar *method-functions* (make-hash-table :test #'equal)
  "Hash table mapping methods to the function implementing that method.

   Each key is of the form (NAME QUALIFIERS SPECIALIZERS) where NAME
   is the name of the generic function, QUALIFIERS is the method
   qualifier list and SPECIALIZERS is the argument specializer list.")

(defun gf-methods (gf-name)
  "Returns the method information for the generic function GF-NAME. A
   hash-table mapping lists of specializers to `METHOD-INFO' objects
   is returned."

  (gethash gf-name *generic-function-table*))

(defun ensure-gf-methods (gf-name)
  "Ensures that a method table for the generic function GF-NAME
   exists, in *GENERIC-FUNCTION-TABLE*."

  (ensure-gethash gf-name *generic-function-table* (make-hash-table :test #'equal)))

(defun gf-method (gf-name spec)
  "Returns the `METHOD-INFO', of the method with specifier (qualifier
   and specializer list) SPEC of the generic function GF-NAME."

  (aand (gf-methods gf-name) (gethash spec it)))

(defun (setf gf-method) (value gf-name spec)
  "Sets the method info, to VALUE, for the method with
   specifier (qualifier and specializer list) SPEC of the generic
   function GF-NAME."

  (setf (gethash spec (ensure-gf-methods gf-name))
	value))

(defun ensure-method-info (gf-name qualifiers specializers &key body lambda-list remove-on-redefine-p)
  "Ensures that the method table, within *GENERIC-FUNCTION-TABLE*, of
   the generic function GF-NAME contains a method with qualifiers
   QUALIFIERS, specializers SPECIALIZERS, lambda-list LAMBDA-LIST and
   body BODY. If the table does not contain a method for those
   specializers, a new `METHOD-INFO' object is created."

  (aprog1
      (ensure-gethash
       (list qualifiers specializers) (ensure-gf-methods gf-name)

       (make-instance 'method-info
		      :specializers specializers
		      :qualifiers qualifiers
		      :remove-on-redefine-p remove-on-redefine-p))

    (setf (lambda-list it) lambda-list)
    (setf (body it) body)))

(defun remove-defined-methods (name)
  "Remove the methods of a generic-function for which REMOVE-ON-REDEFINE-P is true.

   NAME is the name of the generic function."

  (let ((methods (ensure-gf-methods name)))
    (iter
      (for (key method) in-hashtable methods)
      (when (remove-on-redefine-p method)
	(remhash key methods)))))

(defun method-spec (method-info)
  "Return the method specifier, i.e. the key used within the method table.

   METHOD-INFO is the `METHOD-INFO' object of the method."

  (with-slots (qualifiers specializers) method-info
    (list qualifiers specializers)))

(defun find-method% (gf qualifiers specializers)
  #+clisp
  (find-method
   gf qualifiers
   (mapcar
    (lambda (specializer)
      (match specializer
	((list 'eql value)
	 (intern-eql-specializer value))

	(_
	 (find-class specializer nil))))
    specializers)
   nil)

  #-clisp (find-method gf qualifiers specializers nil))

(defun info-for-method (gf-name method)
  "Return the `METHOD-INFO' object for a method.

   GF-NAME is the name of the generic function.

   METHOD is the method object.

   Returns the corresponding `METHOD-INFO' object or NIL if there is
   no information about the method."

  (->> (method-specializers method)
       (mapcar #'specializer->cl)
       (list (method-qualifiers method))
       (gf-method gf-name)))


;;; DEFMETHOD and DEFGENERIC Macros

(defmacro defmethod (name &rest args)
  (with-gensyms (method)
    `(let ((,method
            (walk-environment
              (c2mop:defmethod ,name ,@args))))

       ,(handler-case
	    (multiple-value-bind (qualifiers specializers lambda-list body)
	        (parse-method args)

              (declare (ignore qualifiers))

              `(progn
	         ,(make-add-method-info name method body)
                 ,(make-static-dispatch name lambda-list specializers)))

	  (error (e)
            (simple-style-warning "Error parsing DEFMETHOD form:~%~2T~a" e)))

       ,method)))

(defmacro defgeneric (name (&rest lambda-list) &rest options)
  (let ((combination 'standard)
        (combination-options))

    (with-gensyms (gf method)
      (progn
        `(let ((,gf
                (walk-environment
                  (c2mop:defgeneric ,name ,lambda-list ,@options))))

           (add-compiler-macro ',name)
           (remove-defined-methods ',name)

           ,@(handler-case
                 (append
                  (mappend
	           (lambda-match
	             ((list* :method args)
	              (multiple-value-bind (qualifiers specializers lambda-list body)
		          (parse-method args)

		        (list `(let ((,method (find-method% ,gf ',qualifiers ',specializers)))
                                 ,(make-add-method-info name method body :remove-on-redefine-p t))

                              (make-static-dispatch name lambda-list specializers))))

                     ((list* :method-combination name options)
                      (setf combination name)
                      (setf combination-options options)))

	           options)

                  `((setf (combination-options ',name)
                          '(,combination ,combination-options))))

	       (error (e)
                 (simple-style-warning "Error parsing DEFGENERIC form:~%~a" e)))

           ,gf)))))

(defun make-add-method-info (name method body &key remove-on-redefine-p)
  "Create a form which adds the method information to the method table.

   NAME is the name of the generic function.

   METHOD is a variable storing the method object.

   BODY is the method body.

   REMOVE-ON-REDEFINE-P is a flag for whether the method should be
   removed when the generic function is redefined."

  `(ensure-method-info
    ',name
    (method-qualifiers ,method)
    (mapcar #'specializer->cl (method-specializers ,method))
    :body ',body
    :lambda-list (method-lambda-list ,method)
    :remove-on-redefine-p ,remove-on-redefine-p))

(defun add-compiler-macro (name)
  "Add compiler-macro to generic-function."

  (aif (aand (compiler-macro-function name)
             (not (eq it #'static-dispatch)))
      (simple-style-warning
       "Could not enable static dispatch for ~a: Function already has a compiler-macro."
       name)
      (setf (compiler-macro-function name) #'static-dispatch)))


;;; Parsing Method Definitions

(defun parse-method (def)
  (nlet parse ((def def) (qualifiers nil))
    (ematch def
      ((list* (and (type symbol) qualifier) def)
       (parse def (cons qualifier qualifiers)))

      ((list* (and (type proper-list) lambda-list) body)
       (multiple-value-call #'values
         (reverse qualifiers)
         (parse-method-lambda-list lambda-list)
         body)))))

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


;;; Method Ordering

(defun compute-applicable-methods% (name types)
  "Determine the applicable methods for given argument types.

   NAME is the name of the generic-function.

   TYPES is the list of types of the generic-function required
   arguments.

   Returns the sorted list of applicable methods. Returns NIL if there
   are no applicable methods, or if the generic-function contains a
   method for which there is no `METHOD-INFO' object."

  (flet ((check-method (method)
           (or (info-for-method name method)
               (error "Missing method info for method ~s" method))))

    (let* ((gf (fdefinition name))
	   (precedence (precedence-order (generic-function-lambda-list gf) (generic-function-argument-precedence-order gf)))
	   (types (order-by-precedence precedence types))
           (methods (generic-function-methods gf)))

      (when (every #'check-method methods)
        (-<> (order-method-specializers methods precedence)
	     (applicable-methods types)
	     (sort-methods)
	     (mapcar #'cdr <>))))))


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
           (let ((specializers (->> (method-specializers method)
                                    (mapcar #'specializer->cl))))

             (cons (order-by-precedence precedence specializers)
                   method))))
    (mapcar #'order-specializers methods)))

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
	     (cons (car method) method)))

    (->> (mapcar #'copy-specializer methods)
	 (filter-methods types)
	 (mapcar #'cdr))))

(defun sort-methods (methods)
  "Sorts METHODS by specificity."

  (sort methods #'specializer< :key #'car))

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


  (let* ((levels (declaration-information 'optimize env))
         (speed (or (second (assoc 'speed levels)) 1))
         (safety (or (second (assoc 'safety levels)) 1))
         (debug (or (second (assoc 'debug levels)) 1)))

    (and (= speed 3)
         (< safety 3)
         (< debug 3)
         (not
          (eq 'notinline
              (->> (function-information name env)
                   (nth-value 2)
                   (assoc 'inline)
                   (cdr)))))))

(defun should-check-types? (env)
  "Returns true if CHECK-TYPE forms should be added in the body of
   CALL-NEXT-METHOD. CHECK-TYPE forms are added if the priority of
   the SAFETY optimize quality is greater than or equal to the SPEED
   optimize quality in the environment ENV."

  (let* ((optimize (declaration-information 'optimize env))
         (safety (or (second (assoc 'safety optimize)) 1)))
    (plusp safety)))


;;; Inlining

(defvar *current-gf* nil
  "The name of the generic function currently being inlined/statically
   dispatched.")

(defvar *env* nil
  "Environment in which the current generic function is being
   statically dispatched.")

(defvar *full-arg-list-form* nil
  "Bound to a from which constructs the full argument list for use as
   the default argument list in CALL-NEXT-METHOD.")

(defvar *call-args* nil
  "Bound to the argument list of the current generic function call.")

(defstruct temp-method
  "Represents a 'temporary' method created with MAKE-METHOD."

  body)

(defun inline-call (gf methods args types check-types)
  "Inline a call to a generic-function with a given list of methods.

   GF is the generic-function object, of the generic-function call to
   inline.

   METHODS is the sorted list of applicable method objects.

   TYPES is the list of the types of the arguments.

   CHECK-TYPES is a flag for whether, if true, type checks should be
   inserted in the CALL-NEXT-METHOD local function definitions."

  (multiple-value-bind (bindings args declarations)
      (make-argument-bindings args types)

    `(let ,bindings
       ,@(when declarations
           `((declare ,@declarations)))

       (static-dispatch-test-hook)

       ,(wrap-in-macros
         (generic-function-name gf)
         args
         (compute-effective-method% gf methods args)

         :types types
         :check-types check-types))))

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

(defun wrap-in-macros (name args form &key types check-types)
  "Wrap a form in the lexical CALL-METHOD and MAKE-METHOD macros.

   NAME is the name of the generic-function being statically
   dispatched.

   ARGS is the specifier for the arguments of the generic-function
   call.

   FORM is the form which should be inserted in the body of the
   MACROLET.

   TYPES is the argument type list.

   CHECK-TYPES is a flag for whether type checks should be inserted in
   the CALL-NEXT-METHOD definitions.

   Returns a MACROLET form."

  (with-gensyms (method next expandedp env)
    `(macrolet ((call-method (,method &optional ,next &environment ,env)
                  (let ((*current-gf* ',name)
                        (*full-arg-list-form* ',*full-arg-list-form*))

                    (when (consp ,method)
                      (multiple-value-bind (,method ,expandedp)
                          (macroexpand ,method ,env)

                        (when ,expandedp
                          (return-from call-method `(call-method ,,method)))))

                    (etypecase ,method
                      (method
                       (inline-method-form
                        ',name
                        ,method
                        ',args
                        ,next
                        :types ',types
                        :check-types ',check-types))

                      (temp-method
                       (temp-method-body ,method)))))

                (make-method (,method)
                  (make-temp-method :body ,method)))

       ,form)))

(defun inline-method-form (name method args next-methods &key types check-types)
  "Inline a method with the definitions of the CALL-NEXT-METHOD and NEXT-METHOD-P functions.

   NAME is the name of the generic-function.

   METHOD is the method object, of the method to inline.

   ARGS is the specifier for the arguments to the generic-function call.

   NEXT-METHODS is the list of the next method objects.

   TYPES is the argument type list.

   CHECK-TYPES if a flag for whether type checks should be inserted in
   the CALL-NEXT-METHOD definition.

   Returns a form containing the inlined method with the lexical
   CALL-NEXT-METHOD and NEXT-METHOD-P function definitions."

  (destructuring-bind (&optional next-method &rest more-methods) next-methods
    (with-gensyms (next-arg-var next-args)
      `(flet ((call-next-method (&rest ,next-arg-var)
                (let ((,next-args (or ,next-arg-var ,(next-method-default-args args))))
                  (declare (ignorable ,next-args))

                  ,(if next-method
                       (wrap-in-macros
                        name
                        next-args
                        `(call-method ,next-method ,more-methods)
                        :check-types check-types)
                       `(apply #'no-next-method (fdefinition ',name) ,method ,next-args))))

              (next-method-p ()
                ,(and next-method t)))

         (declare (ignorable #'call-next-method #'next-method-p))
         ,(let ((info (info-for-method name method)))
            (if-let (fn (method-function-name name info nil))
              (make-method-function-call fn args `(#'call-next-method ,(and next-method t)))
              (make-inline-method-body info args types check-types)))))))

(defun make-inline-method-body (method args types check-types)
  "Returns the inline method body (without the CALL-NEXT-METHOD and
   NEXT-METHOD-P functions), of a METHOD, when applied on arguments
   ARGS with types TYPES."

  (with-slots (lambda-list specializers body) method
    (let ((lambda-list
           (if (->> (fdefinition *current-gf*)
                    generic-function-lambda-list
                    (member '&allow-other-keys))

               (add-allow-other-keys lambda-list)
               lambda-list)))

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
		     (list (make-type-declarations
                            (subseq lambda-list 0 (length specializers))
                            types)))

	         ,@declarations

	         ,@(when (and (null types) check-types)
		     (make-type-checks lambda-list specializers))

	         ,@forms)))))))

(defun make-method-function-call (function args &optional extra)
  "Generate call to the function which implements a method.

   FUNCTION is the name of the function which implements the method.

   ARGS is the argument specifier of the method. Either a list
   containing the argument forms, or a symbol naming the variable in
   which the argument list is stored.

   EXTRA is a list of extra arguments to pass before ARGS.

   Returns the function call form."

  (etypecase args
    (null
     `(,function ,@extra ,@*call-args*))

    (cons
     `(,function ,@extra ,@args))

    (symbol
     `(apply #',function ,@extra ,args))))

(defun add-allow-other-keys (lambda-list)
  "Add &ALLOW-OTHER-KEYS to a LAMBDA-LIST."

  (multiple-value-bind (required optional rest key allow-other-keys aux keyp)
      (parse-ordinary-lambda-list lambda-list)

    (declare (ignore allow-other-keys))

    (unparse-lambda-list required optional rest key t aux keyp)))

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

  (flet ((remove-nil-sp-var (arg)
           (match arg
             ((list arg init nil)
              (list arg init))

             (_ arg))))

    (append
     required
     (when optional
       (list* '&optional
              (mapcar #'remove-nil-sp-var optional)))
     (when rest
       (list '&rest rest))

     (when keyp
       '(&key))

     (mapcar #'remove-nil-sp-var key)

     (when allow-other-keys
       '(&allow-other-keys))

     (when aux
       (list* '&aux aux)))))

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

(defun make-type-checks (vars types)
  "Returns a list of CHECK-TYPE forms for each variable in VARS and
   corresponding type in TYPES."

  (mapcar (curry #'list 'check-type) vars types))

(defun make-ignorable-declarations (vars)
  "Creates a DECLARE IGNORABLE expression for the variables in VARS."

  `(declare (ignorable ,@vars)))
