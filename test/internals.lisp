;;;; internals.lisp
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

;;;; Unit Tests

(defpackage :static-dispatch/test.internals
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:optima

	:static-dispatch/test
	:fiveam)

  (:shadowing-import-from :fiveam :fail)

  (:import-from
   :static-dispatch
   :match*

   :parse-method

   :*generic-function-table*
   :gf-method
   :gf-methods

   :method-info
   :body
   :lambda-list
   :specializers
   :qualifiers

   :precedence-order
   :order-by-precedence
   :order-method-specializers
   :applicable-methods
   :sort-methods
   :specializer<

   :*current-gf*
   :find-method%
   :inline-call
   :inline-method-form
   :info-for-method
   :block-name
   :static-dispatch-test-hook))

(in-package :static-dispatch/test.internals)


;;; Test suite definition

(def-suite internals
    :description "Test internals of static-dispatching."
    :in static-dispatch)

(in-suite internals)


;;; Utilities

(defun eval-method (method)
  "Evaluate a method definition form while hiding errors and
   warnings."

  (ignore-errors
    (handler-bind ((warning #'muffle-warning))
      (eval method))))


;;; Test DEFMETHOD parsing

(defmacro test-parse (expected method-form)
  `(is (equal
	,expected

	(multiple-value-list
	 (parse-method ,method-form)))))

(test defmethod-parse-required-with-class-specializers
  "Test parsing required arguments with class specializers"

  (test-parse
   '(nil
     (number t character t)
     (a b c d)
     ((declare (ignore d))
      (pprint c)
      (+ a b)))

   '(((a number) b (c character) (d t))
     (declare (ignore d))
     (pprint c)
     (+ a b))))

(test defmethod-parse-required-with-eql-specializers
  "Test parsing required arguments with EQL specializers"

  (test-parse
   '(nil
     (number t character t (eql 3))
     (a b c d e)
     ((declare (ignore d))
      (pprint c)
      (+ a b)))

   '(((a number) b (c character) (d t) (e (eql 3)))
     (declare (ignore d))
     (pprint c)
     (+ a b))))

(test defmethod-parse-optional-rest-keyword
  "Test parsing optional, rest and keyword arguments"

  (let ((required '((a number) b (c character) (d t) (e (eql 3))))
	(other-args '(&optional x (y 1) &rest args &key x (y 'y y-p) (z)))
	(body '((declare (ignore d))
		(pprint c)
		(+ a b))))

    (test-parse
     `(nil
       (number t character t (eql 3))
       (a b c d e ,@other-args)
       ,body)

     `((,@required ,@other-args) ,@body))))

(test defmethod-parse-before-qualifier
  "Test parsing :BEFORE qualifiers"

  (test-parse
   '((:before)
     (t number character)
     (a b c)

     ((pprint c)
      (+ a b)))

   '(:before (a (b number) (c character)) (pprint c) (+ a b))))

(test defmethod-parse-multiple-qualifiers
  "Test parsing :BEFORE qualifiers"

  (test-parse
   '((qual1 qual2 qual3 :around)
     (t number character)
     (a b c)

     ((pprint c)
      (+ a b)))

   '(qual1 qual2 qual3 :around (a (b number) (c character)) (pprint c) (+ a b))))


;;; DEFMETHOD Macro Tests

;; Generic function used in testing the DEFMETHOD macro.

(defgeneric equal? (a b))

;;; DEFMETHOD forms for EQUAL? Methods

(define-symbol-macro equal?-method1
    '(static-dispatch:defmethod equal? ((a number) (b number))
      (= a b)))

(define-symbol-macro equal?-method2
    '(static-dispatch:defmethod equal? (a b)
      (eq a b)))

(define-symbol-macro equal?-method3
    '(static-dispatch:defmethod equal? ((x (eql 'all)) (y t)) t))

(define-symbol-macro equal?-method4
    '(static-dispatch:defmethod equal? :before ((x integer) y)
      (pprint x)
      (pprint y)))

(def-fixture method-table ()
  ;; "Bind *GENERIC-FUNCTION-TABLE* to a new hash-table and surround the
  ;;  body in the dynamic binding context"

  (let ((*generic-function-table* (make-hash-table :test #'equal)))
    (&body)))

;; Test that that the DEFMETHOD macro is adding the method
;; information to the generic function method table.

(test (defmethod-specialized-method :fixture method-table)
  "Test DEFMETHOD with specialized arguments"

  (eval-method equal?-method1)

  ;; Check that a method table was created for EQUAL?
  (is-true (gf-methods 'equal?)
	   "Method table not created for EQUAL?")

  (let ((method-info (gf-method 'equal? '(nil (number number)))))
    ;; Check that the method was added to the method table for
    ;; EQUAL?

    (is (typep method-info 'method-info)
	"Method for (NIL (NUMBER NUMBER)) not a `METHOD-INFO' object. Got:~% ~a"
	method-info)

    (is (equal '(a b) (lambda-list method-info)))
    (is (equal nil (qualifiers method-info)))
    (is (equal '(number number) (specializers method-info)))
    (is (equal (cdddr equal?-method1) (body method-info)))))

(test (defmethod-non-specialized-method :fixture method-table
	:depends-on defmethod-specialized-method)

  "Test DEFMETHOD without specialized arguments"

  ;; Expand previous methods
  (eval-method equal?-method1)

  ;; Expand this method
  (eval-method equal?-method2)

  ;; Check that the (NUMBER NUMBER) method is still in the method
  ;; table
  (is (typep (gf-method 'equal? '(nil (number number))) 'method-info)
      "Method for (NIL (NUMBER NUMBER)) no-longer in method table")

  (let ((method-info (gf-method 'equal? '(nil (t t)))))
    ;; Check that the method was added to the method table for
    ;; EQUAL?

    (is (typep method-info 'method-info)
	"Method for (NIL (NUMBER NUMBER)) not a `METHOD-INFO' object. Got:~%~a"
	method-info)

    (is (equal '(a b) (lambda-list method-info)))
    (is (equal nil (qualifiers method-info)))
    (is (equal '(t t) (specializers method-info)))
    (is (equal (cdddr equal?-method2) (body method-info)))))

(test (defmethod-eql-specializer
	  :fixture method-table
	:depends-on (and defmethod-specialized-method
			 defmethod-non-specialized-method))

  "Test DEFMETHOD with EQL specializers"

  ;; Expand previous methods
  (eval-method equal?-method1)
  (eval-method equal?-method2)

  ;; Evaluate DEFMETHOD form to actually create the method and add
  ;; a method for (EQL ALL) to the generic function table
  (eval-method equal?-method3)

  (let ((method-info (gf-method 'equal? '(nil ((eql all) t)))))
    ;; Check that the method was added to the method table for
    ;; EQUAL?

    (is (typep method-info 'method-info)
	"Method for (NIL ((EQL ALL) T)) not a `METHOD-INFO' object. Got~%~a"
	method-info)

    (is (equal '(x y) (lambda-list method-info)))
    (is (equal nil (qualifiers method-info)))
    (is (equal '((eql all) t) (specializers method-info)))
    (is (equal (cdddr equal?-method3) (body method-info)))))

(test (defmethod-before-qualifier
	  :fixture method-table
	:depends-on (and defmethod-specialized-method
			 defmethod-non-specialized-method
			 defmethod-eql-specializer))

  "Test DEFMETHOD with :BEFORE qualifier"

  ;; Expand previous methods
  (eval-method equal?-method1)
  (eval-method equal?-method2)
  (eval-method equal?-method3)

  ;; Expand current method
  (eval-method equal?-method4)

  ;; Check that previous methods still in method table
  (is (typep (gf-method 'equal? '(nil (number number))) 'method-info)
      "Method for (NIL (NUMBER NUMBER)) no longer in method table.")

  (is (typep (gf-method 'equal? '(nil (t t))) 'method-info)
      "Method for (NIL (T T)) no longer in method table")

  (is (typep (gf-method 'equal? '(nil ((eql all) t))) 'method-info)
      "Method for (NIL (EQL ALL) T) no longer in method table.")

  (let ((method-info (gf-method 'equal? '((:before) (integer t)))))
    (is (typep method-info 'method-info)
	"Method for (:BEFORE (INTEGER T)) not a `METHOD-INFO' object. Got: ~a~%"
	method-info)

    (is (equal '(x y) (lambda-list method-info)))
    (is (equal '(:before) (qualifiers method-info)))
    (is (equal '(integer t) (specializers method-info)))
    (is (equal (cddddr equal?-method4) (body method-info)))))


;;; Specializer Ordering Tests

;; Classes used to test method ordering

(c2mop:defclass person ()
  (name age))

(c2mop:defclass child (person)
  (parent))

;; Tests that the methods are being ordered correctly based on
;; specificity.

(test specializer-order-class-precedence
  "Test specializer ordering based on class precedence"

  (is-true (specializer< '(child t) '(person number)))
  (is-false (specializer< '(person number) '(child t))))

(test specializer-order-same-first-specializer
  "Test specializer ordering with same initial specializers"

  (is-true (specializer< '(child number) '(child t)))
  (is-false (specializer< '(child t) '(child number))))

(test sepcializer-order-initial-unrelated-classes
  "Test specializer ordering on unrelated classes in initial specializers"

  (is-true (specializer< '(child number) '(character t)))
  (is-false (specializer< '(character t) '(child number))))

;; Test ordering when all specializers are unrelated classes. In
;; this case the specializers should be ordered based on the
;; orderings of the names of the classes of the last
;; specializers. This is to ensure that there is a total ordering
;; of class specializers.

(test specializer-order-unrelated-class-names
  "Test specializer ordering based on unrelated class names"

  (is-true (specializer< '(child number) '(character string)))
  (is-false (specializer< '(character string) '(child number))))

(test specializer-order-eql-specializers
  "Test EQL specializer ordering"

  (is-true (specializer< '((eql all) t) '(child number)))
  (is-false (specializer< '(child number) '((eql all) t))))

(test sepcializer-order-same-eql-specializers
  "Test ordering of identical EQL specializers"

  (is-true (specializer< '((eql all) number) '((eql all) t)))
  (is-false (specializer< '((eql all) t) '((eql all) number))))

(test specializer-order-different-eql-specializers
  "Test ordering of different EQL specializers"

  (is-false (specializer< '((eql 1) number) '((eql 2) t))))


;;; Method Ordering Tests

(define-symbol-macro method-order-test-methods
    (mapcar
     #'list
     '((number t)
       (character t)
       (character character)
       (t t)
       (hash-table t)
       (person person)
       (integer t)
       (t (eql 1))
       ((eql x) t)
       ((eql x) string)
       (number number)
       (person child)
       (integer t))))

(test method-order-specializer-order
  "Test method ordering based on specializer ordering"

  (is (equal
       (mapcar
	#'list
	'(((eql x) string)
	  ((eql x) t)
	  (character character)
	  (person child)
	  (person person)
          (character t)
	  (hash-table t)
	  (integer t)
	  (integer t)
	  (number number)
	  (number t)

	  (t (eql 1))
	  (t t)))

       (sort-methods method-order-test-methods))))

(test method-order-applicable-methods
  "Test determining the applicable methods"

  (is (equal
       (mapcar
	#'list
	'((number t)
	  (t t)
	  (integer t)
	  (number number)
	  (integer t)))

       (applicable-methods method-order-test-methods '(integer integer))))

  (is (equal
       (mapcar
	#'list
	'((number t)
	  (t t)
	  (number number)))

       (applicable-methods method-order-test-methods '(number number))))

  (is (equal
       nil
       (applicable-methods method-order-test-methods '(number t))))

  (is (equal
       (mapcar
	#'list
	'((t t)
	  (hash-table t)))

       (applicable-methods method-order-test-methods '(hash-table t))))

  (is (equal
       (mapcar
	#'list
	'((t t)
	  (person person)
	  (person child)))

       (applicable-methods method-order-test-methods '(child child))))

  (is (equal
       (mapcar
	#'list
	'((t t)
	  (person person)))

       (applicable-methods method-order-test-methods '(child person))))

  (is (equal
       (mapcar
	#'list
	'((t t)
	  ((eql x) t)))

       (applicable-methods method-order-test-methods '((eql x) number))))

  ;; Check that if there is not enough type information for an
  ;; argument and not all the specializers for that argument, of
  ;; all methods, are T then APPLICABLE-METHODS returns nil.

  (is (equal
       nil
       (applicable-methods method-order-test-methods '(t t)))))

(test method-order-generic-function-precedence-order
  "Test method ordering by generic function argument precedence order"

  (is (equal '(2 0 1)
	     (precedence-order '(a b c &optional d e) '(c a b))))

  ;; Test ORDER-BY-PRECEDENCE function

  (is (equal '(1 x (+ a b))
	     (order-by-precedence '(2 0 1) '(x (+ a b) 1 "optional arg"))))

  ;; Test ORDER-METHOD-SPECIALIZERS function

  (let* ((name (gensym "TEMP-GF"))
         (methods
          (eval
           `(progn
              (defgeneric ,name (a b c)
                (:argument-precedence-order c a b))

              (list
               (defmethod ,name ((x number) y (z character))
                 (list :m1 x y z))

               (defmethod ,name :before ((a integer) b c)
                          (list :m2 a b c))

               (defmethod ,name (d e f)
                 (list :default d e f)))))))
    (is (equal
         (mapcar
          #'cons
	  '((character number t)
	    (t integer t)
	    (t t t))
          methods)

         (order-method-specializers methods '(2 0 1))))))


;;; Test Method Inlining

(defvar *gensym-map*)

(defun form= (expected got)
  "Returns true if the form GOT is equivalent to EXPECTED. If EXPECTED
   is a symbol beginning with $, it represents a GENSYM'd
   symbol. Further occurrences of EXPECTED will be compared to the
   first value of GOT, to which EXPECTED was compared."

  (match* (got expected)
    (((cons gh gt) (cons eh et))
     (and (form= eh gh)
	  (form= et gt)))

    ((_ (guard expected
               (and (symbolp expected)
                    (starts-with #\$ (symbol-name expected)))))

     (equal got (ensure-gethash expected *gensym-map* got)))

    (((type symbol) (type symbol))
     (eq got expected))

    ((_ _) (equal got expected))))

(defmacro is-form (expected got &rest args)
  `(let ((*gensym-map* (make-hash-table :test #'eq)))
     (is (form= ,expected ,got) ,@args)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-environments.cltl2::disable-walker
    (defgeneric my-equal (x y &optional z))

    (defmethod my-equal ((a number) (b number) &optional (c (* a b)))
      (declare (ignore c))
      (= a b))

    (defmethod my-equal (x y &optional c)
      (declare (ignore c))
      (eq x y))

    (defmethod my-equal :before ((n1 number) n2 &optional (z 'x z-sp))
               (declare (ignore z z-sp))
               (pprint n1)
               (pprint n2))))

(def-fixture inlining-my-equal ()
  (let ((*check-types*)
	(*current-gf* 'my-equal))
    (declare (special *check-types*))
    (&body)))

(test method-inline-block-name
  "Test BLOCK-NAME function"

  (is (equal 'equal? (block-name 'equal?)))
  (is (equal 'field (block-name '(setf field)))))


;; Test the output of the INLINE-METHOD-FORM function

(test (method-inline-one-next-method :fixture inlining-my-equal)
  "Test method inlining with one next method"

  (let ((method1 (find-method% #'my-equal nil '(number number)))
        (method2 (find-method% #'my-equal nil '(t t))))

    (is-form
     `(let (($a1 (+ u v)))
        (declare (type integer $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method1 (,method2))))

     (inline-call
      #'my-equal
      (list method1 method2)
      '((+ u v) 3)
      '(integer integer)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args1)
               (let (($next1 (or $args1 (list g1 3))))
                 (declare (ignorable $next1))
                 (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                            (make-method ($method) . $make-method-def))

                   (call-method ,method2 nil))))

             (next-method-p () t))
        (declare (ignorable #'call-next-method #'next-method-p))

        (block my-equal
          (let* ((a g1)
                 (b 3)
                 (c (* a b)))

            (declare (ignorable a b))
            (declare (type integer a) (type integer b))
            (declare (ignore c))

            (= a b))))

     (inline-method-form
      'my-equal
      method1
      '(g1 3)
      (list method2)
      :types '(integer integer)
      :check-types nil))))

(test (method-inline-no-next-methods :fixture inlining-my-equal)
  "Test method inlining with no next methods"

  (let ((method (find-method% #'my-equal nil '(t t))))

    (is-form
     `(let (($a1 (+ u v)))
        (declare (type integer $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      #'my-equal
      (list method)
      '((+ u v) 3)
      '(integer integer)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list g1 g2))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition 'my-equal) ,method $next)))

	     (next-method-p () nil))
        (declare (ignorable #'call-next-method #'next-method-p))

        (block my-equal
	  (let* ((x g1)
		 (y g2)
		 (c nil))
	    (declare (ignorable x y))
            (declare (ignore c))
	    (check-type x t)
	    (check-type y t)

	    (eq x y))))

     (inline-method-form 'my-equal method '(g1 g2) nil :check-types t))))

(test (method-inline-with-type-checks :fixture inlining-my-equal)
  "Test method inlining with type checks"

  (let ((method (find-method% #'my-equal nil '(t t))))
    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args next-args)))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition 'my-equal) ,method $next)))

	     (next-method-p () nil))
        (declare (ignorable #'call-next-method #'next-method-p))

        (block my-equal
          (destructuring-bind (x y &optional c) next-args
	    (declare (ignorable x y))
            (declare (ignore c))
	    (check-type x t)
	    (check-type y t)

	    (eq x y))))

     (inline-method-form 'my-equal method 'next-args nil :check-types t))))

(test (method-inline-with-before-method :fixture inlining-my-equal)
  "Test method inlining with :BEFORE method"

  (let ((method1 (find-method% #'my-equal nil '(number number)))
        (method2 (find-method% #'my-equal '(:before) '(number t)))
        (method3 (find-method% #'my-equal nil '(t t))))

    (is-form
     `(let (($a1 (f x)) ($a2 (* z 4)))
        (declare (type fixnum $a1)
                 (type integer $a2))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (progn
            (call-method ,method2)
            (call-method ,method1 (,method3)))))

     (inline-call
      #'my-equal
      (list method1 method2 method3)
      '((f x) (* z 4))
      '(fixnum integer)
      nil))))

;;;; Test SETF methods

(test method-inline-setf-method
  "Test inlining of SETF method"

  (let* ((*current-gf* `(setf ,(gensym "FIELD")))
         (method
          (eval
           `(cl-environments.cltl2::disable-walker
              (defgeneric ,*current-gf* (value x &optional y))

              (defmethod ,*current-gf* (x y &optional c)
                (declare (ignore c))
                (setf (car y) x))))))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list a b 3))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

	     (next-method-p () nil))
        (declare (ignorable #'call-next-method #'next-method-p))

        (block ,(second *current-gf*)
          (let* ((x a) (y b) (c 3))
	    (declare (ignorable x y))
            (declare (type t x)
                     (type t y))
            (declare (ignore c))

	    (setf (car y) x))))

     (inline-method-form *current-gf* method '(a b 3) nil :types '(t t)))))

;;;; Test Optional Arguments

(test method-inline-with-optional-arguments
  "Test inlining a method with optional arguments"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                      (defgeneric ,*current-gf* (a &optional b c d)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                     (defmethod ,*current-gf* ((a string) &optional b (c nil c-sp) (d 1 d-sp))
                      (pprint a)
                      (pprint b)
                      (list a b c d c-sp d-sp))))))

    (is-form
     `(let (($a1 (get-s1 s1))
            ($a2 (get-s2 s2))
            ($a3 (get-s3 s3)))
        (declare (type string $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '((get-s1 s1) (get-s2 s2) (get-s3 s3))
      '(string)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
              (let (($next (or $args (list g1 g2 g3))))
        	(declare (ignorable $next))
        	(apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

             (next-method-p () nil))

       (declare (ignorable #'call-next-method #'next-method-p))

       (block ,*current-gf*
         (let* ((a g1)
        	(b g2)
        	(c g3)
        	(c-sp t)
        	(d 1)
        	(d-sp nil))
           (declare (ignorable a))
           (declare (type string a))
           (pprint a)
           (pprint b)
           (list a b c d c-sp d-sp))))

     (inline-method-form *current-gf* method '(g1 g2 g3) nil :types '(string)))))

;;;; Test Rest Arguments

(test method-inline-with-rest-argument-empty
  "Test inlining a method with empty rest argument"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                      (defgeneric ,*current-gf* (a &optional b &rest xs)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                     (defmethod ,*current-gf* ((a number) &optional b &rest xs)
                      (pprint a)
                      (pprint b)
                      (list* a b xs))))))

    (is-form
     `(let (($a1 (something x))
            ($a2 y))
        (declare (type integer $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '((something x) y)
      '(integer)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
               (let (($next (or $args (list a1 a2))))
        	 (declare (ignorable $next))
        	 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

             (next-method-p () nil))

        (declare (ignorable #'call-next-method #'next-method-p))

        (block ,*current-gf*
          (let* ((a a1)
        	 (b a2)
        	 (xs (list)))
            (declare (ignorable a))
            (declare (type integer a))
            (pprint a)
            (pprint b)
            (list* a b xs))))

     (inline-method-form *current-gf* method '(a1 a2) nil :types '(integer)))))

(test method-inline-with-rest-argument
  "Test inlining a method with non-empty rest argument"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                      (defgeneric ,*current-gf* (a &optional b &rest xs)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                     (defmethod ,*current-gf* ((a character) &optional b &rest xs)
                      (pprint a)
                      (pprint b)
                      (list* a b xs))))))

    (is-form
     `(let (($a1 (something a))
            ($a2 b)
            ($a3 c)
            ($a4 d))
        (declare (type character $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '((something a) b c d 1 2 3)
      '(character)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list a1 a2 a3 a4 1 2 3))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

	     (next-method-p () nil))

        (declare (ignorable #'call-next-method #'next-method-p))

        (block ,*current-gf*
	  (let* ((a a1)
		 (b a2)
		 (xs (list a3 a4 1 2 3)))
	    (declare (ignorable a))
	    (declare (type character a))
	    (pprint a)
	    (pprint b)
	    (list* a b xs))))

     (inline-method-form *current-gf* method '(a1 a2 a3 a4 1 2 3) nil :types '(character)))))

;;;; Test Keyword Arguments

(test method-inline-with-key-arguments
  "Test inlining a method with keyword"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                      (defgeneric ,*current-gf* (v1 &key &allow-other-keys)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                     (defmethod ,*current-gf* ((v1 number) &key v2 (v3 1) ((:key123 v4) (+ v1 v2)) (v5 0 v5-sp))
                      (declare (ignore v5-sp))
                      (list v1 v2 v3 v4 v5))))))

    (is-form
     `(let (($a1 a)
            ($a2 b)
            ($a3 c)
            ($a4 (* a b)))
        (declare (type number $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '(a :v2 b :key123 c :v3 (* a b))
      '(number)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list a1 :v2 a2 :key123 a3 :v3 a4))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

	     (next-method-p () nil))

        (declare (ignorable #'call-next-method #'next-method-p))

        (block ,*current-gf*
	  (let* ((v1 a1) (v2 a2) (v3 a4) (v4 a3)
		 (v5 0) (v5-sp nil))
	    (declare (ignorable v1))
	    (declare (type number v1))
            (declare (ignore v5-sp))
	    (list v1 v2 v3 v4 v5))))

     (inline-method-form
      *current-gf* method '(a1 :v2 a2 :key123 a3 :v3 a4) nil
      :types '(number)))))

(test method-inline-with-key-and-rest-arguments
  "Test inlining a method with keyword and rest arguments"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                      (defgeneric ,*current-gf* (a &rest b &key &allow-other-keys)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                     (defmethod ,*current-gf* ((v1 number) &rest all &key v2 (v3 1) ((:key123 v4) (+ v1 v2) v4-sp) (v5 0))
                      (declare (ignore v4-sp all))
                      (list v1 v2 v3 v4 v5))))))

    (is-form
     `(let (($a1 a)
            ($a2 b)
            ($a3 c)
            ($a4 (* a b))
            ($a5 (+ x y z)))
        (declare (type number $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '(a :v2 b :key123 c :v3 (* a b) :other (+ x y z))
      '(number)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list a1 :v2 a2 :key123 a3 :v3 a4 :other a5))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

	     (next-method-p () nil))

        (declare (ignorable #'call-next-method #'next-method-p))

        (block ,*current-gf*
	  (let* ((v1 a1)
		 (all (list :v2 a2 :key123 a3 :v3 a4 :other a5))
		 (v2 a2)
		 (v3 a4)
		 (v4 a3)
		 (v4-sp t)
		 (v5 0))
	    (declare (ignorable v1))
	    (declare (type number v1))
            (declare (ignore v4-sp all))
	    (list v1 v2 v3 v4 v5))))

     (inline-method-form
      *current-gf* method
      '(a1 :v2 a2 :key123 a3 :v3 a4 :other a5)
      nil
      :types '(number)))))

(test method-inline-with-key-and-rest-arguments-fail-1
  "Test inlining a method with failed keyword argument destructuring"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                      (defgeneric ,*current-gf* (a &rest b &key v2 v3 key123 v5)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                     (defmethod ,*current-gf* ((v1 number) &rest all &key v2 (v3 1) ((:key123 v4) (+ v1 v2) v4-sp) (v5 0))
                      (declare (ignore v4-sp all))
                      (list v1 v2 v3 v4 v5))))))

    (is-form
     `(let (($a1 a)
            ($a2 b)
            ($a3 c)
            ($a4 (* a b))
            ($a5 (+ x y z)))
        (declare (type number $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '(a :v2 b :key123 c :v3 (* a b) :other (+ x y z))
      '(number)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list a1 :v2 a2 :key123 a3 :v3 a4 :other a5))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

	     (next-method-p () nil))

        (declare (ignorable #'call-next-method #'next-method-p))

        (block ,*current-gf*
	  (destructuring-bind (v1 &rest all &key v2 (v3 1) ((:key123 v4) (+ v1 v2) v4-sp) (v5 0))
	      (list a1 :v2 a2 :key123 a3 :v3 a4 :other a5)

	    (declare (ignorable v1))
	    (declare (type number v1))
            (declare (ignore v4-sp all))
	    (list v1 v2 v3 v4 v5))))

     (inline-method-form
      *current-gf* method
      '(a1 :v2 a2 :key123 a3 :v3 a4 :other a5)
      nil
      :types '(number)))))

(test method-inline-with-key-and-rest-arguments-fail-2
  "Test inlining a method with failed keyword argument destructuring"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                      (defgeneric ,*current-gf* (a &rest b &key &allow-other-keys)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                     (defmethod ,*current-gf* ((v1 number) &rest all &key v2 (v3 1) ((:key123 v4) (+ v1 v2) v4-sp) (v5 0) &allow-other-keys)
                      (declare (ignore v4-sp all))
                      (list v1 v2 v3 v4 v5))))))

    (is-form
     `(let (($a1 a)
            ($a2 key1)
            ($a3 b)
            ($a4 key2)
            ($a5 c)
            ($a6 key3)
            ($a7 (* a b)))
        (declare (type number $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '(a key1 b key2 c key3 (* a b))
      '(number)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list a1 a2 a3 a4 a5 a6 a7))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

	     (next-method-p () nil))

        (declare (ignorable #'call-next-method #'next-method-p))

        (block ,*current-gf*
	  (destructuring-bind (v1 &rest all &key ((:v2 v2) nil) ((:v3 v3) 1) ((:key123 v4) (+ v1 v2) v4-sp) ((:v5 v5) 0) &allow-other-keys)
	      (list a1 a2 a3 a4 a5 a6 a7)

	    (declare (ignorable v1))
	    (declare (type number v1))
            (declare (ignore v4-sp all))
	    (list v1 v2 v3 v4 v5))))

     (inline-method-form
      *current-gf* method
      '(a1 a2 a3 a4 a5 a6 a7)
      nil
      :types '(number)))))

(test method-inline-with-aux-arguments
  "Test inlining a method with auxiliary arguments"

  (let* ((*current-gf* (gensym "INLINE-TEST"))
         (gf (eval `(cl-environments.cltl2::disable-walker
                     (defgeneric ,*current-gf* (a b)))))
         (method (eval
                  `(cl-environments.cltl2::disable-walker
                    (defmethod ,*current-gf* ((x number) y &aux (sum (+ x y)))
                      sum)))))

    (is-form
     `(let (($a1 (get-arg a)))
        (declare (type integer $a1))

        (static-dispatch-test-hook)

        (macrolet ((call-method ($m1 &optional $mnext &environment $env) . $call-method-def)
                   (make-method ($method) . $make-method-def))

          (call-method ,method nil)))

     (inline-call
      gf
      (list method)
      '((get-arg a) 1)
      '(integer)
      nil))

    (is-form
     `(flet ((call-next-method (&rest $args)
	       (let (($next (or $args (list a1 1))))
		 (declare (ignorable $next))
		 (apply #'no-next-method (fdefinition ',*current-gf*) ,method $next)))

	     (next-method-p () nil))

	(declare (ignorable #'call-next-method #'next-method-p))

	(block ,*current-gf*
	  (let* ((x a1) (y 1) (sum (+ x y)))
	    (declare (ignorable x y))
	    (declare (type integer x))
	    sum)))

     (inline-method-form *current-gf* method '(a1 1) nil :types '(integer)))))
