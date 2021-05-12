;;;; next-methods-defs.lisp
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

;;;; Generic function and method definitions for the CALL-NEXT-METHOD
;;;; tests

(defpackage :static-dispatch/test.next-methods
  (:use :static-dispatch-cl
	:alexandria
	:arrows

	:fiveam
	:static-dispatch/test))

(in-package :static-dispatch/test.next-methods)

;;; Definitions used by tests

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

;; Currently not used as NO-NEXT-METHOD is not implemented
;; consistently across implementations.

;; Test functions for tests which check that NO-NEXT-METHOD tests is
;; called

;; (defgeneric bar (x y)
;;   (:method ((x number) (y number))
;;     (list 'number (call-next-method (1+ x) (1+ y))))

;;   (:method ((x string) (y string))
;;     (list 'string (call-next-method))))

;; (defmethod no-next-method ((gf (eql (fdefinition 'bar))) method &rest args)
;;   (declare (ignore method))
;;   (list 'no-next-method 'bar args))

;;; Macros

(defmacro pass-through (form)
  "Expands to FORM unchanged."
  form)

(define-symbol-macro a-number 2)

(defconstant +a-constant+ 10)
