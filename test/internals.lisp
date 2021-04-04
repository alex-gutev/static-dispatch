;;;; internals.lisp
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

;;;; Unit Tests

(defpackage :static-dispatch-test-internals
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:trivia

	:prove)

  (:import-from
   :static-dispatch
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

(in-package :static-dispatch-test-internals)

(plan nil)

(subtest "DEFMETHOD form parser"
  ;; Test parse-method function

  ;; Simple method with only required parameters and class specializers
  (subtest "Required Arguments with Class Specializers"
    (is-values (parse-method
		'(((a number) b (c character) (d t))
		  (declare (ignore d))
		  (pprint c)
		  (+ a b)))

	       '(nil
		 (number t character t)
		 (a b c d)
		 ((declare (ignore d))
		  (pprint c)
		  (+ a b)))))

  ;; Same method with an EQL specializer

  (subtest "Required Arguments with EQL specializers"
    (is-values (parse-method
		'(((a number) b (c character) (d t) (e (eql 3)))
		  (declare (ignore d))
		  (pprint c)
		  (+ a b)))

	       '(nil
		 (number t character t (eql 3))
		 (a b c d e)
		 ((declare (ignore d))
		  (pprint c)
		  (+ a b)))))

  ;; Other argument types
  (subtest "Optional, Rest and Keyword Arguments"
    (let ((required '((a number) b (c character) (d t) (e (eql 3))))
	  (other-args '(&optional x (y 1) &rest args &key x (y 'y y-p) (z)))
	  (body '((declare (ignore d))
		  (pprint c)
		  (+ a b))))

      (is-values (parse-method `((,@required ,@other-args) ,@body))
		 `(nil
		   (number t character t (eql 3))
		   (a b c d e ,@other-args)
		   ,body))))

  (subtest ":BEFORE Qualifiers"
    (is-values
     (parse-method '(:before (a (b number) (c character)) (pprint c) (+ a b)))

     '(:before
       (t number character)
       (a b c)

       ((pprint c)
	(+ a b)))))

  ;; Qualifiers - Should raise a MATCH-ERROR as qualifiers are not supported yet.

  (subtest "Unsupported Feature Errors"
    (is-error (parse-method '(and (a (b number) (c character)) (pprint c) (+ a b)))
	      'trivia:match-error)))

;; Generic function used in testing the DEFMETHOD macro.

(defgeneric equal? (a b))

(subtest "DEFMETHOD macro"
  ;; Test that that the DEFMETHOD macro is adding the method
  ;; information to the generic function method table.

  (let ((*generic-function-table* (make-hash-table :test #'eq)))
    (subtest "Add Specialized Method"
      (let* ((method `(static-dispatch:defmethod equal? ((a number) (b number))
			(= a b))))

	(macroexpand method)

	;; Check that a method table was created for EQUAL?
	(ok (gf-methods 'equal?))

	(let ((method-info (gf-method 'equal? '(nil (number number)))))
	  ;; Check that the method was added to the method table for
	  ;; EQUAL?

	  (ok method-info)
	  (is (lambda-list method-info) '(a b))
	  (is (qualifier method-info) nil)
	  (is (specializers method-info) '(number number))
	  (is (body method-info) (cdddr method)))))

    (subtest "Add Non-Specialized Method"
      (let* ((method `(static-dispatch:defmethod equal? (a b)
			(eq a b))))

	(macroexpand method)

	;; Check that the (NUMBER NUMBER) method is still in the method
	;; table
	(ok (gf-method 'equal? '(nil (number number))))

	(let ((method-info (gf-method 'equal? '(nil (t t)))))
	  ;; Check that the method was added to the method table for
	  ;; EQUAL?

	  (ok method-info)
	  (is (lambda-list method-info) '(a b))
	  (is (qualifier method-info) nil)
	  (is (specializers method-info) '(t t))
	  (is (body method-info) (cdddr method)))))

    (subtest "Add Method with EQL specializers"
      (let* ((method `(static-dispatch:defmethod equal? ((x (eql 'all)) (y t)) t)))

	;; Evaluate DEFMETHOD form to actually create the method and add
	;; a method for (EQL ALL) to the generic function table
	(eval method)

	(let ((method-info (gf-method 'equal? '(nil ((eql all) t)))))
	  ;; Check that the method was added to the method table for
	  ;; EQUAL?

	  (ok method-info)
	  (is (lambda-list method-info) '(x y))
	  (is (qualifier method-info) nil)
	  (is (specializers method-info) '((eql all) t))
	  (is (body method-info) (cdddr method)))))

    ;; Test Qualifiers

    (subtest "Add Method with :BEFORE qualifier"
      (let ((method '(static-dispatch:defmethod equal? :before ((x integer) y)
		      (pprint x)
		      (pprint y))))

	(macroexpand method)

	;; Check Existing Methods
	(ok (gf-method 'equal? '(nil (number number))))
	(ok (gf-method 'equal? '(nil (t t))))
	(ok (gf-method 'equal? '(nil ((eql 'all) t))))

	(let ((method-info (gf-method 'equal? '(:before (integer t)))))
	  (ok method-info)
	  (is (lambda-list method-info) '(x y))
	  (is (qualifier method-info) :before)
	  (is (specializers method-info) '(integer t))
	  (is (body method-info) (cddddr method)))))

    ;; Test Unsupported Features

    (macroexpand '(static-dispatch:defmethod equal? and (x y)
		   (pprint x)
		   (pprint y)))


    ;; Check that the method table for equal? was removed as
    ;; method combinations are not supported.

    (is (gf-methods 'equal?) nil)

    ;; Check that even if a new method with no qualifiers is defined,
    ;; the method table will not be recreated.

    (macroexpand '(static-dispatch:defmethod (x y)
		   (pprint x)
		   (pprint y)))

    (is (gf-methods 'equal?) nil)))


;; Classes used to test method ordering

(c2mop:defclass person ()
  (name age))

(c2mop:defclass child (person)
  (parent))


(subtest "Method Ordering"
  ;; Tests that the methods are being ordered correctly based on
  ;; specificity.

  (subtest "Specializer Ordering"
    ;; Test SPECIALIZE< function

    ;; Test ordering based on class precedence
    (ok (specializer< '(child t) '(person number)))
    (is (specializer< '(person number) '(child t)) nil)

    ;; Test ordering when first argument specializers are identical
    (ok (specializer< '(child number) '(child t)))
    (is (specializer< '(child t) '(child number)) nil)

    ;; Test ordering when initial specializers are unrelated classes.
    (ok (specializer< '(child number) '(character t)))
    (is (specializer< '(character t) '(child number)) nil)

    ;; Test ordering when all specializers are unrelated classes. In
    ;; this case the specializers should be ordered based on the
    ;; orderings of the names of the classes of the last
    ;; specializers. This is to ensure that there is a total ordering
    ;; of class specializers.
    (ok (specializer< '(child number) '(character string)))
    (is (specializer< '(character string) '(child number)) nil)

    ;; Test EQL specializers
    (ok (specializer< '((eql all) t) '(child number)))
    (is (specializer< '(child number) '((eql all) t)) nil)

    ;; Test ordering of identical EQL specializers
    (ok (specializer< '((eql all) number) '((eql all) t)))
    (is (specializer< '((eql all) t) '((eql all) number)) nil)

    ;; Test ordering of different EQL specializers
    (is (specializer< '((eql 1) number) '((eql 2) t)) nil))

  (let* ((methods
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
	     (:before (integer t))))))

    (subtest "Method ordering based on specializer ordering"
      ;; Test SORT-METHODS function

      (is (sort-methods (copy-list methods))
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
	     (nil (t t))))))

    (subtest "Determining the applicable methods"
      ;; Test APPLICABLE-METHODS function

      (is (applicable-methods methods '(integer integer))
	  (mapcar
	   #'list
	   '((nil (number t))
	     (nil (t t))
	     (nil (integer t))
	     (:before (number number))
	     (:before (integer t)))))

      (is (applicable-methods methods '(number number))
	  (mapcar
	   #'list
	   '((nil (number t))
	     (nil (t t))
	     (:before (number number)))))

      (is (applicable-methods methods '(number t)) nil)

      (is (applicable-methods methods '(hash-table t))
	  (mapcar
	   #'list
	   '((nil (t t))
	     (nil (hash-table t)))))

      (is (applicable-methods methods '(child child))
	  (mapcar
	   #'list
	   '((nil (t t))
	     (nil (person person))
	     (nil (person child)))))

      (is (applicable-methods methods '(child person))
	  (mapcar
	   #'list
	   '((nil (t t))
	     (nil (person person)))))

      (is (applicable-methods methods '((eql x) number))
	  (mapcar
	   #'list
	   '((nil (t t))
	     (nil ((eql x) t)))))

      ;; Check that if there is not enough type information for an
      ;; argument and not all the specializers for that argument, of
      ;; all methods, are T then APPLICABLE-METHODS returns nil.
      (is (applicable-methods methods '(t t)) nil))))


(subtest "Ordering by generic function argument precedence order"
  ;; Test PRECEDENCE-ORDER function

  (is (precedence-order '(a b c &optional d e) '(c a b)) '(2 0 1))

  ;; Test ORDER-BY-PRECEDENCE function

  (is (order-by-precedence '(2 0 1) '(x (+ a b) 1 "optional arg")) '(1 x (+ a b)))

  ;; Test ORDER-METHOD-SPECIALIZERS function

  (is (order-method-specializers
       (mapcar
	#'list
	'((nil (number t character))
	  (:before (integer t t))
	  (nil (t t t))))
       '(2 0 1))
      (mapcar
       #'list
       '((nil (character number t))
	 (:before (t integer t))
	 (nil (t t t))))))

(defvar *gensym-map*)

(defun form= (got expected)
  "Returns true if the form GOT is equivalent to EXPECTED. If EXPECTED
   is a symbol beginning with $, it represents a GENSYM'd
   symbol. Further occurrences of EXPECTED will be compared to the
   first value of GOT, to which EXPECTED was compared."

  (match* (got expected)
    (((cons gh gt) (cons eh et))
     (and (form= gh eh)
	  (form= gt et)))

    (((type symbol) (type symbol))
     (if (starts-with #\$ (symbol-name expected))
	 (eq got (ensure-gethash expected *gensym-map* got))
	 (eq got expected)))

    ((_ _) (equal got expected))))

(defmacro is-form (got expected &rest args)
  `(let ((*gensym-map* (make-hash-table :test #'eq)))
     (is ,got ,expected :test #'form= ,@args)))


(subtest "Method inlining"
  ;; Test the BLOCK-NAME function

  (is (block-name 'equal?) 'equal?)
  (is (block-name '(setf field)) 'field)

  ;; Test the output of the INLINE-METHODS function

  (let ((*check-types*)
	(*current-gf* 'equal?)
	(method1 (make-instance 'method-info
				:body '((= a b))
				:lambda-list '(a b &optional c)
				:qualifier nil
				:specializers '(number number)))
	(method2 (make-instance 'method-info
				:body '((eq x y))
				:lambda-list '(x y &optional c)
				:qualifier nil
				:specializers '(t t)))

	(method3 (make-instance 'method-info
				:body '((pprint n1)
					(pprint n2))
				:lambda-list '(n1 n2 &optional z)
				:qualifier :before
				:specializers '(number t))))

    (declare (special *check-types*))

    (subtest "One Next Method"
      (is-form
       (inline-methods (list method1 method2) '((+ u v) 3) nil '(integer integer))
       `(flet ((call-next-method (&rest $args1)
		 (let (($next1 (or $args1 (list (+ u v) 3))))
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
	    (destructuring-bind (a b &optional c) (list (+ u v) 3)
	      (declare (ignorable a b))
	      (declare (type integer a) (type integer b))

	      (= a b))))))

    (subtest "No Next Methods"
      (is-form
       (inline-methods (list method2) '(y z) t)
       `(flet ((call-next-method (&rest $args)
		 (let (($next (or $args (list y z))))
		   (declare (ignorable $next))
		   (apply #'no-next-method 'equal? nil $next)))

	       (next-method-p () nil))
	  (declare (ignorable #'call-next-method #'next-method-p))

	  (block equal?
	    (destructuring-bind (x y &optional c) (list y z)
	      (declare (ignorable x y))
	      (check-type x t)
	      (check-type y t)

	      (eq x y))))))

    (subtest "With Type Checks"
      (is-form
       (inline-methods (list method1 method2) '((+ u v) 3) t '(number number))

       `(flet ((call-next-method (&rest $args1)
		 (let (($next1 (or $args1 (list (+ u v) 3))))
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
	    (destructuring-bind (a b &optional c) (list (+ u v) 3)
	      (declare (ignorable a b))
	      (declare (type number a) (type number b))

	      (= a b))))))

    (subtest "With :BEFORE Method"
      (is-form
       (inline-methods (list method3 method1 method2) '((f x) (* z 4)) nil '(fixnum integer))

       `(progn
	  (flet ((call-next-method (&rest $args1)
		  (declare (ignore $args1))
		  (error 'illegal-call-next-method-error :method-type :before))

		(next-method-p () nil))
	   (declare (ignorable #'call-next-method #'next-method-p))

	   (block equal?
	     (destructuring-bind (n1 n2 &optional z) (list (f x) (* z 4))
	       (declare (ignorable n1 n2))
	       (declare (type fixnum n1) (type integer n2))
	       (pprint n1)
	       (pprint n2))))

	  (flet ((call-next-method (&rest $args2)
		   (let (($next2 (or $args2 (list (f x) (* z 4)))))
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
	      (destructuring-bind (a b &optional c) (list (f x) (* z 4))
		(declare (ignorable a b))
		(declare (type fixnum a) (type integer b))

		(= a b)))))))

    ;; ;; Test SETF methods

    (subtest "SETF Method"
      (let ((*current-gf* '(setf field)))
	(is-form
	 (inline-methods (list method2) '(a b) nil '(t t))
	 `(flet ((call-next-method (&rest $args)
		   (let (($next (or $args (list a b))))
		     (declare (ignorable $next))
		     (apply #'no-next-method '(setf field) nil $next)))

		 (next-method-p () nil))

	    (declare (ignorable #'call-next-method #'next-method-p))

	    (block field
	      (destructuring-bind (x y &optional c) (list a b)
		(declare (ignorable x y))
		(declare (type t x) (type t y))
		(eq x y)))))))))

(finalize)
