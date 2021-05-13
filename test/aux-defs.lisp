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

;;;; Generic function and method definitions for the auxiliary method
;;;; tests.

(defpackage :static-dispatch/test.aux
  (:use :static-dispatch-cl
	:alexandria
	:arrows

	:fiveam
	:static-dispatch/test))

(in-package :static-dispatch/test.aux)

;;; Definitions used by tests



;;; Generic Function with Auxiliary Methods

(defgeneric my-eq (a b))

(defmethod my-eq ((a number) (b number))
  (= a b))

(defmethod my-eq (a b)
  (eq a b))


(defmethod my-eq :before ((a number) (b number))
  (format t "Before Numbers: ~a = ~a. " a b))

(defmethod my-eq :before ((a integer) (b integer))
  (format t "Before Integer: ~a = ~a. " a b))

(defmethod my-eq :after ((a number) (b number))
  (format t "After Numbers: ~a = ~a. " a b))

(defmethod my-eq :after ((a integer) (b integer))
  (format t "After Integer: ~a = ~a. " a b))

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
  (format t "BAZ Before all: ~a ~a. " x (next-method-p)))

(defmethod baz :before ((x integer))
  (format t "BAZ Before INTEGER: ~a ~a. " x (next-method-p))
  (call-next-method))

(defmethod baz :after ((x array))
  (format t "BAZ After ARRAY: ~a ~a. " x (next-method-p)))

(defmethod baz :after ((x string))
  (format t "BAZ After STRING: ~a ~a. " x (next-method-p))
  (call-next-method x))

;;; Macros

(defmacro pass-through (form)
  "Expands to FORM unchanged."
  form)

(define-symbol-macro a-number 2)

(defconstant +a-constant+ 10)
