;;;; method-functions-defs.lisp
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

;;;; Generic function and method definitions for static overloading
;;;; tests

(defpackage :static-dispatch/test.method-functions
  (:use :static-dispatch-cl
	:alexandria
	:arrows

	:fiveam
	:static-dispatch/test))

(in-package :static-dispatch/test.method-functions)

;;; Definitions used by tests

;;; Generic function methods which make use of CALL-NEXT-METHOD and
;;; NEXT-METHOD-P, and have keyword arguments.

(defgeneric foo (a b &key &allow-other-keys))

(defmethod foo ((a number) (b number) &key)
  (list 'number (next-method-p) (list a b)))

(defmethod foo ((a integer) (b integer) &key)
  (list 'integer (next-method-p)
	(call-next-method (1+ a) (1+ b))))

(defmethod foo ((a float) (b float) &key)
  (list 'float (next-method-p)
	(call-next-method)))

(defmethod foo ((a string) (b string) &key)
  (list 'string (next-method-p)
	(list a b)))

(defmethod foo (a b &key (default-type 'other))
  (list default-type (next-method-p)
	(list a b)))

;;; Generic functions with auxiliary methods

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

;;; Macros

(defmacro pass-through (form)
  "Expands to FORM unchanged."
  form)

(define-symbol-macro a-number 2)

(defconstant +a-constant+ 10)
