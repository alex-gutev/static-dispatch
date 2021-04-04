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

(defpackage :static-dispatch-test-aux
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:trivia

	:prove
	:static-dispatch-test-util))

(in-package :static-dispatch-test-aux)

(named-readtables:in-readtable :interpol-syntax)

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

;;; Macros

(defmacro pass-through (form)
  "Expands to FORM unchanged."
  form)

(define-symbol-macro a-number 2)

(defconstant +a-constant+ 10)


;;; Tests

(plan nil)

(subtest "Auxiliary Methods"
  (subtest "AROUND Methods"
    (locally (declare (inline my-eq))
      (test-dispatch (my-eq 1/2 0.5) '(:around-number t))
      (test-dispatch (my-eq 1 2) '(:around-integer (:around-number nil)))
      (test-dispatch (my-eq "x" 'x) nil)
      (test-dispatch (my-eq 133 133) :special-number)))

  (subtest "BEFORE and AFTER Methods"
    (locally (declare (inline my-eq))
      (is-print (my-eq 1/2 2/3) #?"Before Numbers: 1/2 = 2/3\nAfter Numbers: 1/2 = 2/3\n")
      (is-print (my-eq 1 2) #?"Before Integer: 1 = 2\nBefore Numbers: 1 = 2\nAfter Numbers: 1 = 2\nAfter Integer: 1 = 2\n")

      ;; Test that :BEFORE and :AFTER methods are not called when not
      ;; applicable.
      (is-print (my-eq 'x 'y) ""))))

(finalize)
