;;;; methods.lisp
;;;;
;;;; Copyright 2019 Alexander Gutev
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

;;;; Generic functions used in the interface tests.

(defpackage :static-dispatch-interface-test
  (:use :static-dispatch-cl
	:alexandria
	:cl-arrows
	:trivia

	:prove))

(in-package :static-dispatch-interface-test)

(defgeneric add (a b))

(defmethod add ((a number) (b number))
  (list 'number (+ a b)))

(defmethod add ((a string) (b string))
  (list 'string a b))

(defmethod add (a b)
  (list a b))


;;; The following generic function has a compiler macro which simply
;;; returns the form as is. The purpose of this test is to ensure that
;;; static-dispatch does not replace existing compiler macros.

(defgeneric f (x))

(define-compiler-macro f (&whole form &rest args)
  (declare (ignore args))
  form)

(defmethod f ((x number))
  x)

(defmethod f ((x t))
  nil)
