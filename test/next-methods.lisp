;;;; next-methods.lisp
;;;;
;;;; Copyright 2019-2021 Alexander Gutev
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

;;;; These tests test the use of STATIC-DISPATCH rather than it's
;;;; internals. Specifically:
;;;;
;;;;  1. That Generic functions are actually statically dispatched
;;;;     when declared INLINE.
;;;;
;;;;  2. That the result of the generic function call is correct
;;;;     regardless of whether they are statically dispatched or not.
;;;;
;;;; This test suite tests CALL-NEXT-METHOD and NEXT-METHOD-P
;;;;
;;;; NOTE:
;;;;
;;;;  If a test of the form "F is statically dispatched" fails, this
;;;;  means that STATIC-DISPATCH was unable to choose the appropriate
;;;;  method for F at compile-time, thus will fallback to dynamic
;;;;  dispatch. Programs using STATIC-DISPATCH will still function
;;;;  correctly, provided the remaining tests don't fail, however they
;;;;  will not be able to make use of statically dispatched generic
;;;;  functions on the implementation on which the test fails. In this
;;;;  case generic functions fallback to the standard dynamic
;;;;  dispatch.

(defpackage :static-dispatch-test-next-methods
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:trivia

	:prove
	:static-dispatch-test-util))

(in-package :static-dispatch-test-next-methods)


;;; Generic function methods which make use of CALL-NEXT-METHOD and
;;; NEXT-METHOD-P

(defgeneric foo (a b))

(defmethod foo ((a number) (b number))
  (list 'number (next-method-p) (list a b)))

(defmethod foo ((a integer) (b integer))
  (list 'integer (next-method-p)
	(call-next-method (1+ a) (1+ b))))

(defmethod foo ((a float) (b float))
  (list 'float (next-method-p)
	(call-next-method)))

(defmethod foo ((a string) (b string))
  (list 'string (next-method-p)
	(list a b)))

(defmethod foo (a b)
  (list 'other (next-method-p)
	(list a b)))


(defgeneric bar (x y)
  (:method ((x number) (y number))
    (list 'number (call-next-method (1+ x) (1+ y))))

  (:method ((x string) (y string))
    (list 'string (call-next-method))))

(defmethod no-next-method ((gf (eql 'bar)) method &rest args)
  (declare (ignore method))
  (list 'no-next-method gf args))

;;; Macros

(defmacro pass-through (form)
  "Expands to FORM unchanged."
  form)

(define-symbol-macro a-number 2)

(defconstant +a-constant+ 10)


;;; Tests

;;; Inhibit notes on SBCL
#+sbcl (declaim (optimize sb-ext:inhibit-warnings))

(plan nil)

(subtest "Test CALL-NEXT-METHOD and NEXT-METHOD-P"
  (subtest "Integer Arguments"
    (let ((x-int 5) (y-int 10))
      (declare (type integer x-int y-int)
	       (optimize speed)
	       (inline foo))

      (test-dispatch
       (foo 1 2)
       '(integer t (number t (2 3))))

      (test-dispatch
       (foo x-int y-int)
       '(integer t (number t (6 11))))

      (test-dispatch
       (foo a-number +a-constant+)
       '(integer t (number t (3 11))))))

  (subtest "Float Arguments"
    (let ((x 0.5) (y 2.5))
      (declare (type float x y)
	       (optimize speed)
	       (inline foo))

      (test-dispatch
       (foo (pass-through 1.5) 3.125)
       '(float t (number t (1.5 3.125))))

      (test-dispatch
       (foo x y)
       '(float t (number t (0.5 2.5))))))

  (subtest "Number Arguments"
    (let ((x 1) (y 3/2))
      (declare (type integer x)
	       (type number y)
	       (optimize speed)
	       (inline foo))

      (test-dispatch
       (foo 1 5/6)
       '(number t (1 5/6)))

      (test-dispatch
       (foo x y)
       '(number t (1 3/2)))))

  (subtest "String Arguments"
    (let ((hello "hello"))
      (declare (type string hello)
	       (optimize speed)
	       (inline foo))

      (test-dispatch
       (foo hello "world")
       '(string t ("hello" "world")))

      (test-dispatch
       (foo (pass-through "hello") "bye")
       '(string t ("hello" "bye")))))

  (subtest "Other Arguments"
    (locally (declare (inline foo)
		      (optimize speed))

      (test-dispatch (foo 'x 0) '(other nil (x 0)))
      (test-dispatch (foo (pass-through "hello") +a-constant+)
		     (list 'other nil (list "hello" +a-constant+)))))

  (subtest "No Next Method"
    (locally (declare (inline bar) (optimize speed))
      (test-dispatch
       (bar 1 2)
       '(number (no-next-method bar (2 3))))

      (test-dispatch
       (bar "abc" "def")
       '(string (no-next-method bar ("abc" "def")))))))

(finalize)
