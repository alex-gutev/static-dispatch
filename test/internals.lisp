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
   :qualifier

   :precedence-order
   :order-by-precedence
   :order-method-specializers
   :applicable-methods
   :sort-methods
   :specializer<

   :*current-gf*
   :inline-methods
   :block-name))

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
   '(:before
     (t number character)
     (a b c)

     ((pprint c)
      (+ a b)))

   '(:before (a (b number) (c character)) (pprint c) (+ a b))))

(test defmethod-unsupported-qualifier-error
  "Test error signalled when parsing unsupported qualifier"

  (signals optima:match-error
    (parse-method '(and (a (b number) (c character)) (pprint c) (+ a b)))))


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

(define-symbol-macro equal?-method5
    '(static-dispatch:defmethod equal-2 and (x y)
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
    (is (equal nil (qualifier method-info)))
    (is (equal '(number number) (specializers method-info)))
    (is (equal (cdddr equal?-method1) (body method-info)))))

(test (defmethod-non-specialized-method :fixture method-table
	:depends-on defmethod-specialized-method)

  "test DEFMETHOD without specialized arguments"

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
    (is (equal nil (qualifier method-info)))
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
    (is (equal nil (qualifier method-info)))
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

  (let ((method-info (gf-method 'equal? '(:before (integer t)))))
    (is (typep method-info 'method-info)
	"Method for (:BEFORE (INTEGER T)) not a `METHOD-INFO' object. Got: ~a~%"
	method-info)

    (is (equal '(x y) (lambda-list method-info)))
    (is (equal :before (qualifier method-info)))
    (is (equal '(integer t) (specializers method-info)))
    (is (equal (cddddr equal?-method4) (body method-info)))))

(test (defmethod-unsupported-qualifier
	  :fixture method-table
	:depends-on (and defmethod-specialized-method
			 defmethod-non-specialized-method
			 defmethod-eql-specializer
			 defmethod-before-qualifier))

  "Test DEFMETHOD with unsupported qualifier"

  ;; Expand previous methods
  (eval-method equal?-method1)
  (eval-method equal?-method2)
  (eval-method equal?-method3)
  (eval-method equal?-method4)

  ;; Copy over generic function table from equal?

  (setf (gethash 'equal-2 *generic-function-table*)
	(gf-methods 'equal?))

  (eval-method '(static-dispatch:defgeneric equal-2 (x y)
		 (:method-combination and)))

  (eval-method equal?-method5)

  ;; Check that the method table for equal-2 was removed as
  ;; method combinations are not supported.

  (is-false (gf-methods 'equal-2)
	    "Method table for EQUAL-2 not removed.")

  ;; Check that even if a new method with no qualifiers is defined,
  ;; the method table will not be recreated.

  (eval-method '(static-dispatch:defmethod equal-2 (x y)
		 (pprint x)
		 (pprint y)))

  (is-false (gf-methods 'equal-2)
	    "New method added to removed method table for EQUAL-2"))


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
     '((nil (number t))
       (:before (character t))
       (nil (character character))
       (nil (t t))
       (nil (hash-table t))
       (nil (person person))
       (nil (integer t))
       (nil (t (eql 1)))
       (nil ((eql x) t))
       (nil ((eql x) string))
       (:before (number number))
       (nil (person child))
       (:before (integer t)))))

(test method-order-specializer-order
  "Test method ordering based on specializer ordering"

  (is (equal
       (mapcar
	#'list
	'((:before (character t))
	  (:before (integer t))
	  (:before (number number))
	  (nil ((eql x) string))
	  (nil ((eql x) t))
	  (nil (character character))
	  (nil (person child))
	  (nil (person person))
	  (nil (hash-table t))
	  (nil (integer t))
	  (nil (number t))

	  (nil (t (eql 1)))
	  (nil (t t))))

       (sort-methods method-order-test-methods))))

(test method-order-applicable-methods
  "Test determining the applicable methods"

  (is (equal
       (mapcar
	#'list
	'((nil (number t))
	  (nil (t t))
	  (nil (integer t))
	  (:before (number number))
	  (:before (integer t))))

       (applicable-methods method-order-test-methods '(integer integer))))

  (is (equal
       (mapcar
	#'list
	'((nil (number t))
	  (nil (t t))
	  (:before (number number))))

       (applicable-methods method-order-test-methods '(number number))))

  (is (equal
       nil
       (applicable-methods method-order-test-methods '(number t))))

  (is (equal
       (mapcar
	#'list
	'((nil (t t))
	  (nil (hash-table t))))

       (applicable-methods method-order-test-methods '(hash-table t))))

  (is (equal
       (mapcar
	#'list
	'((nil (t t))
	  (nil (person person))
	  (nil (person child))))

       (applicable-methods method-order-test-methods '(child child))))

  (is (equal
       (mapcar
	#'list
	'((nil (t t))
	  (nil (person person))))

       (applicable-methods method-order-test-methods '(child person))))

  (is (equal
       (mapcar
	#'list
	'((nil (t t))
	  (nil ((eql x) t))))

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

  (is (equal
       (mapcar
	#'list
	'((nil (character number t))
	  (:before (t integer t))
	  (nil (t t t))))

       (order-method-specializers
	(mapcar
	 #'list
	 '((nil (number t character))
	   (:before (integer t t))
	   (nil (t t t))))
	'(2 0 1)))))


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

    (((type symbol) (type symbol))
     (if (starts-with #\$ (symbol-name expected))
	 (eq got (ensure-gethash expected *gensym-map* got))
	 (eq got expected)))

    ((_ _) (equal got expected))))

(defmacro is-form (expected got &rest args)
  `(let ((*gensym-map* (make-hash-table :test #'eq)))
     (is (form= ,expected ,got) ,@args)))

(define-symbol-macro method-inlining-method1
    (make-instance 'method-info
		   :body '((= a b))
		   :lambda-list '(a b &optional c)
		   :qualifier nil
		   :specializers '(number number)))

(define-symbol-macro method-inlining-method2
    (make-instance 'method-info
		   :body '((eq x y))
		   :lambda-list '(x y &optional c)
		   :qualifier nil
		   :specializers '(t t)))

(define-symbol-macro method-inlining-method3
    (make-instance 'method-info
		   :body '((pprint n1)
			   (pprint n2))
		   :lambda-list '(n1 n2 &optional z)
		   :qualifier :before
		   :specializers '(number t)))

(def-fixture inlining-equal? ()
  (let ((*check-types*)
	(*current-gf* 'equal?))
    (declare (special *check-types*))
    (&body)))

(test method-inline-block-name
  "Test BLOCK-NAME function"

  (is (equal 'equal? (block-name 'equal?)))
  (is (equal 'field (block-name '(setf field)))))

;; Test the output of the INLINE-METHODS function

(test (method-inline-one-next-method :fixture inlining-equal?)
  "Test method inlining with one next method"

  (is-form
   '(let (($a1 (+ u v)))
     (declare (type integer $a1))

     (flet ((call-next-method (&rest $args1)
	      (let (($next1 (or $args1 (list $a1 3))))
		(declare (ignorable $next1))
		(flet ((call-next-method (&rest $args2)
			 (let (($next2 (or $args2 $next1)))
			   (declare (ignorable $next2))
			   (apply #'no-next-method 'equal? nil $next2)))

		       (next-method-p () nil))

		  (declare (ignorable #'call-next-method #'next-method-p))

		  (block equal?
		    (destructuring-bind (x y &optional c) $next1
		      (declare (ignorable x y))
		      (eq x y))))))

	    (next-method-p () t))
       (declare (ignorable #'call-next-method #'next-method-p))

       (block equal?
	 (let* ((a $a1)
		(b 3)
		(c nil))

	   (declare (ignorable a b))
	   (declare (type integer a) (type integer b))

	   (= a b)))))

   (inline-methods (list method-inlining-method1 method-inlining-method2)
		   '((+ u v) 3) nil '(integer integer))))

(test (method-inline-no-next-methods :fixture inlining-equal?)
  "Test method inlining with no next methods"

  (is-form
   '(let (($a1 y)
	  ($a2 z))

     (flet ((call-next-method (&rest $args)
	      (let (($next (or $args (list $a1 $a2))))
		(declare (ignorable $next))
		(apply #'no-next-method 'equal? nil $next)))

	    (next-method-p () nil))
       (declare (ignorable #'call-next-method #'next-method-p))

       (block equal?
	 (let* ((x $a1)
		(y $a2)
		(c nil))
	   (declare (ignorable x y))
	   (check-type x t)
	   (check-type y t)

	   (eq x y)))))

   (inline-methods (list method-inlining-method2) '(y z) t)))

(test (method-inline-with-type-checks :fixture inlining-equal?)
  "Test method inlining with type checks"

  (is-form
   '(let (($a1 (+ u v)))
     (declare (type number $a1))

     (flet ((call-next-method (&rest $args1)
	      (let (($next1 (or $args1 (list $a1 3))))
		(declare (ignorable $next1))
		(flet ((call-next-method (&rest $args2)
			 (let (($next2 (or $args2 $next1)))
			   (declare (ignorable $next2))
			   (apply #'no-next-method 'equal? nil $next2)))

		       (next-method-p () nil))

		  (declare (ignorable #'call-next-method #'next-method-p))

		  (block equal?
		    (destructuring-bind (x y &optional c) $next1
		      (declare (ignorable x y))
		      (check-type x t)
		      (check-type y t)
		      (eq x y))))))

	    (next-method-p () t))

       (declare (ignorable #'call-next-method #'next-method-p))

       (block equal?
	 (let* ((a $a1)
		(b 3)
		(c nil))

	   (declare (ignorable a b))
	   (declare (type number a) (type number b))

	   (= a b)))))

   (inline-methods (list method-inlining-method1 method-inlining-method2)
		   '((+ u v) 3) t '(number number))))

(test (method-inline-with-before-method :fixture inlining-equal?)
  "Test method inlining with :BEFORE method"

  (is-form
   '(let (($a1 (f x))
	  ($a2 (* z 4)))
     (declare (type fixnum $a1)
      (type integer $a2))

     (progn
       (flet ((call-next-method (&rest $args1)
		(declare (ignore $args1))
		(error 'illegal-call-next-method-error :method-type :before))

	      (next-method-p () nil))
	 (declare (ignorable #'call-next-method #'next-method-p))

	 (block equal?
	   (let* ((n1 $a1) (n2 $a2) (z nil))
	     (declare (ignorable n1 n2))
	     (declare (type fixnum n1) (type integer n2))
	     (pprint n1)
	     (pprint n2))))

       (flet ((call-next-method (&rest $args2)
		(let (($next2 (or $args2 (list $a1 $a2))))
		  (declare (ignorable $next2))
		  (flet ((call-next-method (&rest $args3)
			   (let (($next3 (or $args3 $next2)))
			     (declare (ignorable $next3))
			     (apply #'no-next-method 'equal? nil $next3)))

			 (next-method-p () nil))

		    (declare (ignorable #'call-next-method #'next-method-p))

		    (block equal?
		      (destructuring-bind (x y &optional c) $next2
			(declare (ignorable x y))
			(eq x y))))))

	      (next-method-p () t))

	 (declare (ignorable #'call-next-method #'next-method-p))

	 (block equal?
	   (let* ((a $a1) (b $a2) (c nil))
	     (declare (ignorable a b))
	     (declare (type fixnum a) (type integer b))

	     (= a b))))))

   (inline-methods (list method-inlining-method3
			 method-inlining-method1
			 method-inlining-method2)

		   '((f x) (* z 4)) nil '(fixnum integer))))

;; ;; Test SETF methods

(test (method-inline-setf-method :fixture inlining-equal?)
  "Test inlining of SETF method"

  (let ((*current-gf* '(setf field)))
    (is-form
     '(let (($a1 a)
	    ($a2 b))
       (declare (type t $a1)
	(type t $a2))

       (flet ((call-next-method (&rest $args)
		(let (($next (or $args (list $a1 $a2))))
		  (declare (ignorable $next))
		  (apply #'no-next-method '(setf field) nil $next)))

	      (next-method-p () nil))

	 (declare (ignorable #'call-next-method #'next-method-p))

	 (block field
	   (let* ((x $a1) (y $a2) (c nil))
	     (declare (ignorable x y))
	     (declare (type t x) (type t y))
	     (eq x y)))))

     (inline-methods (list method-inlining-method2) '(a b) nil '(t t)))))
