;;;; aux.lisp
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
;;;; This test suite tests auxiliary (:BEFORE, :AFTER and :AROUND)
;;;; methods.
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
;;;;
;;;;  Similarly failures of the form is expected to raise a condition
;;;;  ILLEGAL-CALL-NEXT-METHOD-ERROR/NO-PRIMARY-METHOD-ERROR indicate
;;;;  that the generic functions were not statically dispatched and
;;;;  hence these conditions are not raised but the builtin CLOS
;;;;  conditions are raised.

(defpackage :static-dispatch-test-aux
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:trivia

	:prove
	:static-dispatch-test-util))

(in-package :static-dispatch-test-aux)

(named-readtables:in-readtable :interpol-syntax)

;;; Inhibit notes on SBCL
#+sbcl (declaim (optimize sb-ext:inhibit-warnings))

;;; Generic Function with Auxiliary Methods

(defgeneric my-eq (a b))

(defmethod my-eq ((a number) (b number))
  (= a b))

(defmethod my-eq (a b)
  (eq a b))


(defmethod my-eq :before ((a number) (b number))
  (format t "Before Numbers: ~a = ~a~%" a b))

(defmethod my-eq :before ((a integer) (b integer))
  (format t "Before Integer: ~a = ~a~%" a b))

(defmethod my-eq :after ((a number) (b number))
  (format t "After Numbers: ~a = ~a~%" a b))

(defmethod my-eq :after ((a integer) (b integer))
  (format t "After Integer: ~a = ~a~%" a b))

(defmethod my-eq :around ((a number) (b number))
  (list :around-number (call-next-method)))

(defmethod my-eq :around ((a integer) (b integer))
  (if (= a b 133)
      :special-number
      (list :around-integer (call-next-method a b))))


;;; Generic Functions with no primary methods

(defgeneric foo (x)
  (:method :before (x)
    (format t "FOO Before: ~x" x)))

(defgeneric bar (x)
  (:method :after ((x integer))
     (format t "BAR After: ~x" x)))

(defmethod bar :around ((x t))
  (list 'around-bar (call-next-method)))


;;; Generic Function with before and after methods that call
;;; CALL-NEXT-METHOD:

(defgeneric baz (x)
  (:method (x) x))

(defmethod baz :before (x)
  (format t "BAZ Before all: ~a ~a~%" x (next-method-p)))

(defmethod baz :before ((x integer))
  (format t "BAZ Before INTEGER: ~a ~a~%" x (next-method-p))
  (call-next-method))

(defmethod baz :after ((x array))
  (format t "BAZ After ARRAY: ~a ~a~%" x (next-method-p)))

(defmethod baz :after ((x string))
  (format t "BAZ After STRING: ~a ~a~%" x (next-method-p))
  (call-next-method x))

;;; Macros

(defmacro pass-through (form)
  "Expands to FORM unchanged."
  form)

(define-symbol-macro a-number 2)

(defconstant +a-constant+ 10)


;;; Tests

;; Enable static dispatch
(enable-static-dispatch my-eq foo bar baz)

(plan nil)

(subtest "Auxiliary Methods"
  (subtest "AROUND Methods"
    (locally (declare (inline my-eq)
		      (optimize speed))

      (test-dispatch (my-eq 1/2 0.5) '(:around-number t))
      (test-dispatch (my-eq 1 2) '(:around-integer (:around-number nil)))
      (test-dispatch (my-eq "x" 'x) nil)
      (test-dispatch (my-eq 133 133) :special-number)))

  (subtest "BEFORE and AFTER Methods"
    (locally (declare (inline my-eq)
		      (optimize speed))

      (is-print (my-eq 1/2 2/3) #?"Before Numbers: 1/2 = 2/3\nAfter Numbers: 1/2 = 2/3\n")
      (is-print (my-eq 1 2) #?"Before Integer: 1 = 2\nBefore Numbers: 1 = 2\nAfter Numbers: 1 = 2\nAfter Integer: 1 = 2\n")

      ;; Test that :BEFORE and :AFTER methods are not called when not
      ;; applicable.
      (is-print (my-eq 'x 'y) "")))

  (subtest "No Primary Method"
    (subtest "BEFORE Method"
      (locally (declare (inline foo) (optimize speed))
	(is-print
	 (handler-case (foo "x")
	   (no-primary-method-error () nil))
	 #?"FOO Before: x")

	(test-error (foo 1) no-primary-method-error)))

    (subtest "AFTER Method"
      (locally (declare (inline bar) (optimize speed))
	(is-print
	 (handler-case (bar 1)
	   (no-primary-method-error () nil))
	 "")

	(test-error (bar 5) no-primary-method-error)))

    (subtest "AROUND Method"
      (locally (declare (inline bar) (optimize speed))
	(test-error (bar "hello") no-primary-method-error)
	(test-error (bar 10) no-primary-method-error))))

  (subtest "CALL-NEXT-METHOD and NEXT-METHOD-P from BEFORE and AFTER methods"
    (subtest "BEFORE Method"
      (locally (declare (inline baz) (optimize speed))
	(is-print (baz 'x) #?"BAZ Before all: X NIL\n")

	(is-print
	 (handler-case
	     (baz 1)
	   (illegal-call-next-method-error () nil))
	 #?"BAZ Before INTEGER: 1 NIL\n")

	(test-error (baz 14) illegal-call-next-method-error)))

    (subtest "AFTER Method"
      (locally (declare (inline baz) (optimize speed))
	(is-print (baz #(1 2 3))
		  #?"BAZ Before all: #(1 2 3) NIL\nBAZ After ARRAY: #(1 2 3) NIL\n")

	(is-print
	 (handler-case
	     (baz "abcd")
	   (illegal-call-next-method-error () nil))

	 #?"BAZ Before all: abcd NIL\nBAZ After ARRAY: abcd NIL\nBAZ After STRING: abcd NIL\n")

	(test-error (baz "xyz") illegal-call-next-method-error)))))

(finalize)
