;;;; test.lisp
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

(defpackage :static-dispatch-test
  (:use :common-lisp
	:prove

	:alexandria)

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

   :order-by-precedence
   :order-method-specializers
   :applicable-methods
   :sort-methods
   :specializer<))

(in-package :static-dispatch-test)

(plan nil)

(subtest "DEFMETHOD form parser"
  ;;; Test parse-method function

  ;; Simple method with only required parameters and class specializers
  (is-values (parse-method
	      '(((a number) b (c character) (d t))
		(declare (ignore d))
		(pprint c)
		(+ a b)))

	     '((number t character t)
	       (a b c d)
	       ((declare (ignore d))
		(pprint c)
		(+ a b))))

  ;; Same method with an EQL specializer

  (is-values (parse-method
	      '(((a number) b (c character) (d t) (e (eql 3)))
		(declare (ignore d))
		(pprint c)
		(+ a b)))

	     '((number t character t (eql 3))
	       (a b c d e)
	       ((declare (ignore d))
		(pprint c)
		(+ a b))))

  ;; Other argument types

  (let ((required '((a number) b (c character) (d t) (e (eql 3))))
	(other-args '(&optional x (y 1) &rest args &key x (y 'y y-p) (z)))
	(body '((declare (ignore d))
		(pprint c)
		(+ a b))))

    (is-values (parse-method `((,@required ,@other-args) ,@body))
	       `((number t character t (eql 3))
		 (a b c d e ,@other-args)
		 ,body)))

  ;; Qualifiers - Should raise a MATCH-ERROR as qualifiers are not supported yet.

  (is-error (parse-method '(:before (a (b number) (c character)) (pprint c) (+ a b)))
	    'trivia:match-error)

  (is-error (parse-method '(and (a (b number) (c character)) (pprint c) (+ a b)))
	    'trivia:match-error))

;; Generic function used in testing the DEFMETHOD macro.

(defgeneric equal? (a b))

(subtest "DEFMETHOD macro"
  ;;; Test that that the DEFMETHOD macro is adding the method
  ;;; information to the generic function method table.

  (let ((*generic-function-table* (make-hash-table :test #'eq)))
    (let* ((method `(static-dispatch:defmethod equal? ((a number) (b number))
		      (= a b))))

      (macroexpand method)

      ;; Check that a method table was created for EQUAL?
      (ok (gf-methods 'equal?))

      (let ((method-info (gf-method 'equal? '(number number))))
	;; Check that the method was added to the method table for
	;; EQUAL?

	(ok method-info)
	(is (lambda-list method-info) '(a b))
	(is (specializers method-info) '(number number))
	(is (body method-info) (cdddr method))))

    (let* ((method `(static-dispatch:defmethod equal? (a b)
		      (eq a b))))

      (macroexpand method)

      ;; Check that the (NUMBER NUMBER) method is still in the method
      ;; table
      (ok (gf-method 'equal? '(number number)))

      (let ((method-info (gf-method 'equal? '(t t))))
	;; Check that the method was added to the method table for
	;; EQUAL?

	(ok method-info)
	(is (lambda-list method-info) '(a b))
	(is (specializers method-info) '(t t))
	(is (body method-info) (cdddr method))))

    (let* ((method `(static-dispatch:defmethod equal? ((x (eql 'all)) (y t)) t)))

      ;; Evaluate DEFMETHOD form to actually create the method and add
      ;; a method for (EQL ALL) to the generic function table
      (eval method)

      (let ((method-info (gf-method 'equal? '((eql all) t))))
	;; Check that the method was added to the method table for
	;; EQUAL?

	(ok method-info)
	(is (lambda-list method-info) '(x y))
	(is (specializers method-info) '((eql all) t))
	(is (body method-info) (cdddr method))))

    ;; Test Qualifiers

    (macroexpand '(static-dispatch:defmethod equal? :before (x y)
		   (pprint x)
		   (pprint y)))



    ;; Check that the method table for equal? was removed as
    ;; qualifiers are not supported.

    (is (gf-methods 'equal?) nil)

    ;; Check that even if a new method with no qualifiers is defined,
    ;; the method table will not be recreated.

    (macroexpand '(static-dispatch:defmethod :before (x y)
		   (pprint x)
		   (pprint y)))

    (is (gf-methods 'equal?) nil)))


;; Classes used to test method ordering

(defclass person ()
  (name age))

(defclass child (person)
  (parent))


(subtest "Method Ordering"
  ;;; Tests that the methods are being ordered correctly based on
  ;;; specificity.

  (subtest "Specializer Ordering"
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
    (is (specializer< '((eql 1) number) '((eql 2) t)) nil)))


(finalize)
