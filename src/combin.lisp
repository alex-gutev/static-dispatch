;;;; combin.lisp
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

;;;; Method Combinations

;;;; The method combination system has to be reimplemented for static
;;;; dispatching, as the output of the functions in the standard and
;;;; MOP is not standardized and varies tremendously between
;;;; implementations.

(in-package #:static-dispatch)

(define-condition unknown-method-combination (error)
  ((combination :initarg :combination
                :reader combination
                :documentation "The method combination"))

  (:report
   (lambda (e s)
     (with-slots (combination) e
       (format s "Unknown method combination: ~s." combination))))

  (:documentation
   "Error condition representing a method-combination which has not
    been defined through STATIC-DISPATCH-CL:DEFINE-METHOD-COMBINATION,
    and hence static-dispatch doesn't know about it."))

(define-condition combin-missing-required (error)
  ((combination :initarg :combination
                :reader combination
                :documentation "Method combination name")

   (group-spec :initarg :group-spec
               :reader group-spec
               :documentation "Method group specifier"))

  (:report
   (lambda (e s)
     (with-slots (combination group-spec) e
       (format s "No methods matching required group ~a in method combination ~a."
               group-spec combination))))

  (:documentation
   "Error condition representing the error of no methods being matched
    to a required group specifier in a method combination."))

(define-condition user-method-combination-error (error)
  ((gf-name :initarg :gf-name
            :reader gf-name
            :documentation "Name of the generic function")

   (combination :initarg :combination
                :reader combination
                :documentation "Name of the method combination")

   (message :initarg :message
            :reader message
            :documentation "The error message"))

  (:report
   (lambda (e s)
     (with-slots (gf-name combination message) e
       (format s "Error computing effective method of ~s with method combination ~a:~%~2T~a"
               gf-name combination message))))

  (:documentation
   "User method combination error condition signalled with
    METHOD-COMBINATION-ERROR function."))

(define-condition user-invalid-method-error (user-method-combination-error)
  ((the-method :initarg :the-method
               :reader the-method
               :documentation "The invalid method"))

  (:report
   (lambda (e s)
     (with-slots (gf-name combination message the-method) e
       (format s "Invalid method ~s for method combination ~a of ~s:~%~2T~a"
               the-method combination gf-name message))))

  (:documentation
   "Error condition representing a method whose qualifiers do not
    match any group specifier. Signalled with the INVALID-METHOD-ERROR
    function."))


(defvar *method-combination-functions* (make-hash-table :test #'eq)
  "Hash-table mapping method-combination names to the functions which
   implement them for the static-dispatch system.

   This is necessary since the built in COMPUTE-EFFECTIVE-METHOD does
   not have a standardized output and varies considerably between
   implementations, hence the functionality has to be reimplemented
   for static-dispatch.")

(defvar *gf-method-combination-options* (make-hash-table :test #'equal)
  "Hash-table mapping generic function names to the list (COMBINATION
   OPTIONS) where COMBINATION is the name of the generic-function's
   method-combination and OPTIONS is the list of options given to the
   method-combination. ")

(defun combination-function (name)
  "Retrieve the function implementing the method-combination
   COMBINATION."

  (gethash name *method-combination-functions*))

(defun (setf combination-function) (value name)
  "Set the combination function implementing a method-combination."

  (setf (gethash name *method-combination-functions*) value))

(defun combination-options (gf-name)
  "Retrieve the method combination name and options of the generic
   function with name GF-NAME."

  (gethash gf-name *gf-method-combination-options*))

(defun (setf combination-options) (options gf-name)
  "Set the method combination name and options for a given
   generic function."

  (setf (gethash gf-name *gf-method-combination-options*)
        options))

(defun compute-effective-method% (gf methods args)
  "Static dispatch version of CLOSER-MOP:COMPUTE-EFFECTIVE-METHOD.

   Compute the effective method of a generic-function for a given list
   of methods.

   Calls the function implementing the generic function's
   method-combination for the static-dispatch system. For this to
   happen, the generic function must be known (declared with
   STATIC-DISPATCH:DEFGENERIC) as well as its
   method-combination (defined with
   STATIC-DISPATCH:DEFINE-METHOD-COMBINATION).

   GF is the generic-function object.

   METHODS is the list of method objects for the applicable methods,
   ordered with the most specific method first.

   ARGS is the specifier for the arguments to the generic function
   call."

  (multiple-value-bind (options known?)
      (combination-options (generic-function-name gf))

    (destructuring-bind (combination options) options
      (let* ((combin-fn (combination-function combination)))
        (check-type combin-fn function)

        (unless known?
          (error "Unknown generic function: ~s." (generic-function-name gf)))

        (compute-effective-method%% gf combin-fn options methods args)))))

(defun compute-effective-method%% (gf combin-fn options methods args)
  "Compute the effective method given a method-combination function.

   GF is the generic-function object.

   COMBIN-FN is the method-combination function.

   METHODS is the list of method objects for the applicable methods,
   ordered with the most specific method first.

   ARGS is the specifier for the arguments to the generic function
   call."

  (funcall combin-fn gf options methods args))


;;; Defining Method Combinations

(defvar *current-method-combination* nil
  "When inside the dynamic extent of a method combination function,
   set to a CONS of the form (GF . COMBINATION) where name is the name
   of the generic function and COMBINATION is the name of the method
   combination.")

(defmacro call-method (method methods)
  `(cl:call-method ,method ,methods))

(defmacro make-method (form)
  `(cl:make-method ,form))

(defun method-combination-error (control &rest args)
  (if *current-method-combination*
      (destructuring-bind (gf . combination) *current-method-combination*
        (error 'user-method-combination-error
               :gf-name gf
               :combination combination
               :message (apply #'format nil control args)))

      (apply #'cl:method-combination-error control args)))

(defun invalid-method-error (method control &rest args)
  (if *current-method-combination*
      (destructuring-bind (gf . combination) *current-method-combination*
        (error 'user-invalid-method-error
               :gf-name gf
               :combination combination
               :the-method method
               :message (apply #'format nil control args)))

      (apply #'cl:invalid-method-error control args)))

(defmacro define-method-combination (name &rest args)
  `(progn
     (define-method-combination% ,name ,@args)
     (c2mop:define-method-combination ,name ,@args)))

(defmacro define-method-combination% (name &rest args)
  "Define a method-combination for the static-dispatch system only.

   Arguments are of the same form as to CL:DEFINE-METHOD-COMBINATION."

  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf
      (combination-function ',name)
      ,(match args
         ((or nil
              (list* (and (type symbol) (not nil)) _))

          (destructuring-bind (&key (operator name) (identity-with-one-argument nil))
              args

            `(method-combination-function-short ,name ,operator ,identity-with-one-argument)))

         (_
          `(method-combination-function ,name ,@args))))))

(defmacro method-combination-function-short (name operator optimize)
  "Generate a method-combination function for the short-form of
   CL:DEFINE-METHOD-COMBINATION."

  (with-gensyms (order around primary form method)
    `(method-combination-function ,name
         (&optional (,order :most-specific-first))
         ((,around (:around))
          (,primary (,name) :order ,order :required t))

       (let ((,form (if (or (null ,optimize) (rest ,primary))
                        `(,',operator ,@(mapcar #'(lambda (,method)
                                                  `(call-method ,,method))
                                              ,primary))
                        `(call-method ,(first ,primary)))))

         (if ,around
             `(call-method ,(first ,around)
                           (,@(rest ,around)
                              (make-method ,,form)))
             ,form)))))

(defmacro method-combination-function (name (&rest lambda-list) (&rest group-specs) &body body)
  "Generate a method-combination function for the long-form of
   CL:DEFINE-METHOD-COMBINATION."

  (declare (optimize (debug 3)))
  (with-gensyms (methods options args)
    (labels ((bind-groups (groups body)
               (multiple-value-bind (forms decls)
                   (parse-body body :documentation t)

                 (loop
                    for (var . options) in groups
                    for (patterns required order) =
                      (multiple-value-list (parse-group-options options))

                    collect var into group-vars
                    collect patterns into group-patterns
                    collect (or order :most-specific-first) into orders
                    collect `(when (and ,required (null ,var))
                               (error 'combin-missing-required
                                      :combination ',name
                                      :group-spec ',(cons var options)))
                    into checks

                    finally
                      (return
                        `(destructuring-bind ,group-vars
                             (group-combin-methods ,methods ',group-patterns (list ,@orders))

                           ,@decls
                           ,@checks
                           ,@forms)))))

             (parse-group-options (options)
               (loop
                  for (option . options) on options
                  unless (keywordp option) collect option into patterns
                  do
                    (when (keywordp option)
                      (multiple-value-bind (required order)
                          (parse-group-key-options (cons option options))

                        (return (values patterns required order))))

                  finally (return patterns)))

             (parse-group-key-options (options)
               (destructuring-bind (&key required order &allow-other-keys)
                   options

                 (values required order)))

             (gensym-vars (lambda-list)
               (multiple-value-bind (required optional rest key)
                   (parse-ordinary-lambda-list lambda-list)

                 (append
                  (gensym-required required)
                  (gensym-optional optional)
                  (gensym-required (ensure-list rest))
                  (gensym-key key))))

             (gensym-optional (optional)
               (mappend
                (lambda (arg)
                  (destructuring-bind (var init sp) arg
                    (declare (ignore init))

                    (list* (gensym-var var)
                           (gensym-required (ensure-list sp)))))
                optional))

             (gensym-key (key)
               (mappend
                (lambda (arg)
                  (destructuring-bind ((keyword var) init sp) arg
                    (declare (ignore init keyword))

                    (list* (gensym-var var)
                           (gensym-required (ensure-list sp)))))
                key))

             (gensym-required (vars)
               (mapcar #'gensym-var vars))

             (gensym-var (var)
               (cons var (gensym (symbol-name var))))

             (make-destructure-args (gf lambda-list gensyms body)
               (multiple-value-bind (forms decl)
                   (parse-body body :documentation t)

                 `((destructure-combin-lambda-list
                    ,gf ',lambda-list ',gensyms ,args

                    (let ,(loop for (arg . gs) in gensyms collect `(,arg ',gs))
                      ,@decl
                      ,@forms))))))

      (match body
        ((or (list* (list* :arguments arg-lambda-list)
                    body)
             body)

         (match body
           ((or (list* (list :generic-function gf-symbol) body)
                body)

            (let ((gf (or gf-symbol (gensym "GF"))))
              `(lambda (,gf ,options ,methods ,args)
                 (declare (ignorable ,gf ,options ,args))
                 (block ,name
                   (let ((*current-method-combination*
                          (cons (generic-function-name ,gf) ',name)))

                     (destructuring-bind ,lambda-list ,options
                       ,(bind-groups
                         group-specs
                         (if arg-lambda-list
                             (let ((gensyms (gensym-vars arg-lambda-list)))
                               (make-destructure-args gf arg-lambda-list gensyms body))

                             body))))))))))))))

(defun group-combin-methods (methods groups orders)
  "Group the methods according to the method-combination group specifier

   METHODS is the list of generic-function methods.

   GROUPS is the list of patterns of each method group specifier

   ORDERS is a list specifying the order, by which the methods should
   be ordered, of each group specifier."

  (let ((grouped (make-list (length groups) :initial-element nil)))
    (labels ((add-to-group (method)
               (loop
                  with qualifiers = (method-qualifiers method)
                  for i from 0
                  for patterns in groups
                  do
                    (when (in-group? qualifiers patterns)
                      (push method (nth i grouped))
                      (return-from add-to-group)))

               (invalid-method-error method "Method qualifiers %s not applicable to any group."
                                     (method-qualifiers method)))

             (in-group? (qualifiers patterns)
               (some (curry #'matches-pattern? qualifiers) patterns))

             (matches-pattern? (qualifiers pattern)
               (match pattern
                 ((and (type symbol)
                       (not '*)
                       (not nil))

                  (funcall pattern qualifiers))

                 (_ (matches? qualifiers pattern))))

             (matches? (qualifiers pattern)
               (match* (pattern qualifiers)
                 (('* _) t)
                 ((nil nil) t)

                 (((type symbol) (type symbol))
                  (eql pattern qualifiers))

                 (((list* pfirst prest)
                   (list* qualifier qualifiers))

                  (and (matches? qualifier pfirst)
                       (matches? qualifiers prest))))))

      (mapc #'add-to-group methods)

      (loop
         for methods in grouped
         for order in orders
         collect
           (progn
             (check-type order (member :most-specific-first :most-specific-last))
             (if (eq order :most-specific-first)
                 (reverse methods)
                 methods))))))

(defun destructure-combin-lambda-list (gf lambda-list gensyms args body)
  "Emit a form which destructures the argument list in a method-combination.

   GF is the generic-function object.

   LAMBDA-LIST is the :ARGUMENTS lambda-list of the
   method-combination.

   GENSYMS is an association list mapping variable names to the
   gensymed variable names which should be inserted in the resulting
   form.

   ARGS is the arguments specifier of the generic function call.

   BODY is a list of forms which make use of the variables in the
   lambda-list.

   Returns a list of forms which wrap BODY in code that destructures
   the arguments according to LAMBDA-LIST."

  (multiple-value-bind (gf-required gf-optional)
      (parse-ordinary-lambda-list (generic-function-lambda-list gf))

    (multiple-value-bind (cm-required cm-optional cm-rest cm-key allow-other-keys cm-aux)
        (parse-ordinary-lambda-list lambda-list)

      (declare (ignore allow-other-keys))

      (with-gensyms (rest)
        (labels ((gensym-var (var)
                   (cdr (assoc var gensyms)))

                 (gensym-optional (opt)
                   (destructuring-bind (var init sp) opt
                     `(,(gensym-var var) ,init ,@(when sp (list (gensym-var sp))))))

                 (gensym-key (key)
                   (destructuring-bind ((key var) init sp) key
                     `((,key ,(gensym-var var)) ,init ,@(when sp (list (gensym-var sp))))))

                 (bind-required (body)
                   (loop
                      for (arg . args) on cm-required
                      for gf-arg in gf-required
                      collect (list (gensym-var arg) gf-arg) into bindings
                      finally
                        (return
                          `(let (,@bindings ,@(mapcar #'gensym-var args))
                             ,body))))

                 (bind-optional (body)
                   (loop
                      for (arg . args) on cm-optional
                      for gf-arg in gf-optional
                      for (var nil sp) = (gensym-optional arg)
                      collect `(,var ,gf-arg) into bindings
                      if sp collect `(,sp t) into bindings
                      finally
                        (return
                          `(let (,@bindings
                                 ,@(extra-optional-bindings args))
                             ,body))))

                 (extra-optional-bindings (args)
                   (loop
                      for (var init sp) in (mapcar #'gensym-optional args)
                      collect `(,var ,init)
                      if sp collect sp))

                 (bind-rest (body)
                   (if cm-rest
                       `(let ((,cm-rest ,rest))
                          ,body)
                       body))

                 (bind-key (body)
                   (if cm-key
                       `(destructuring-bind (,@(mapcar #'gensym-key cm-key) &allow-other-keys)
                            ,cm-rest

                          ,body)
                       body))

                 (bind-aux (body)
                   (if cm-aux
                       `(let ,cm-aux ,body)
                       body)))

          (destructure-args
           args
           (unparse-lambda-list
            gf-required gf-optional rest nil nil nil nil)

           (-> body
               bind-required
               bind-optional
               bind-rest
               bind-key
               bind-aux
               list)))))))
