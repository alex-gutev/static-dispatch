;;;; compiler-macro.lisp
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
;;;; This test suite tests the interaction between static-dispatch and
;;;; other compiler macros.
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

(defpackage :static-dispatch-test-compiler-macro
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:trivia

	:prove
	:static-dispatch-test-util))

(in-package :static-dispatch-test-compiler-macro)

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

;;; Macros

(defmacro pass-through (form)
  "Expands to FORM unchanged."
  form)

(define-symbol-macro a-number 2)

(defconstant +a-constant+ 10)


;;; Tests

(plan nil)

(subtest "Interaction with Other Compiler Macros"
  (isnt (compiler-macro-function 'f) #'static-dispatch
	"Compiler-Macro-Function of F not replaced by STATIC-DISPATCH")

  ;; Test that the function is not statically dispatched, since it's
  ;; existing compiler macro is not replaced.
  (locally (declare (inline f))
    (test-dispatch (f 1) 1 :static-p nil)
    (test-dispatch (f 'x) nil :static-p nil)))

(finalize)
