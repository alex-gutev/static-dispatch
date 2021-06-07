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

(in-package :static-dispatch/test.aux)


;;; Test suite definition

(def-suite auxiliary-methods
    :description "Test static dispatching with auxiliary methods."
    :in static-dispatch)

(in-suite auxiliary-methods)


;;; Tests

;;; Inhibit notes on SBCL
#+sbcl (declaim (optimize sb-ext:inhibit-warnings))

(test (around-methods :compile-at :run-time)
  "Test static dispatching of :AROUND methods"

  (locally (declare (inline my-eq)
		    (optimize speed #+sbcl sb-ext:inhibit-warnings))

    (test-dispatch (my-eq 1/2 0.5) '(:around-number t))
    (test-dispatch (my-eq 1 2) '(:around-integer (:around-number nil)))
    (test-dispatch (my-eq "x" 'x) nil)
    (test-dispatch (my-eq 133 133) :special-number)))

(test (before-after-methods :compile-at :run-time)
  "Test static dispatching of :BEFORE and :AFTER methods"

  (locally (declare (inline my-eq)
		    (optimize speed #+sbcl sb-ext:inhibit-warnings))

    (is-print (my-eq 1/2 2/3) "Before Numbers: 1/2 = 2/3. After Numbers: 1/2 = 2/3. ")
    (is-print (my-eq 1 2) "Before Integer: 1 = 2. Before Numbers: 1 = 2. After Numbers: 1 = 2. After Integer: 1 = 2. ")

    ;; Test that :BEFORE and :AFTER methods are not called when not
    ;; applicable.
    (is-print (my-eq 'x 'y) "")))

(test (before-method-without-primary :compile-at :run-time)
  "Test static dispatching of :BEFORE method without primary method"

  (locally (declare (inline foo) (optimize speed #+sbcl sb-ext:inhibit-warnings))
    (is-print
     (handler-case (foo "x")
       (no-primary-method-error () nil))
     "FOO Before: x")

    (test-error (foo 1) no-primary-method-error)))

(test (after-method-without-primary :compile-at :run-time)
  "Test static dispatching of :AFTER method without primary method"

  (locally (declare (inline bar) (optimize speed #+sbcl sb-ext:inhibit-warnings))
    (is-print
     (handler-case (bar 1)
       (no-primary-method-error () nil))
     "")

    (test-error (bar 5) no-primary-method-error)))

(test (around-method-without-primary :compile-at :run-time)
  "Test static dispatching of :AROUND method without primary method"

  (locally (declare (inline bar) (optimize speed #+sbcl sb-ext:inhibit-warnings))
    (test-error (bar "hello") no-primary-method-error)
    (test-error (bar 10) no-primary-method-error)))

(test (before-method-call-next-method :compile-at :run-time)
  "Test CALL-NEXT-METHOD from :BEFORE method"

  (locally (declare (inline baz) (optimize speed #+sbcl sb-ext:inhibit-warnings))
    (is-print (baz 'x) "BAZ Before all: X NIL. ")

    (is-print
     (handler-case
	 (baz 1)
       (illegal-call-next-method-error () nil))
     "BAZ Before INTEGER: 1 NIL. ")

    (test-error (baz 14) illegal-call-next-method-error)))

(test (after-method-call-next-method :compile-at :run-time)
  "Test CALL-NEXT-METHOD from :AFTER method"

  (locally (declare (inline baz) (optimize speed #+sbcl sb-ext:inhibit-warnings))
    (is-print (baz #(1 2 3))
	      "BAZ Before all: #(1 2 3) NIL. BAZ After ARRAY: #(1 2 3) NIL. ")

    (is-print
     (handler-case
	 (baz "abcd")
       (illegal-call-next-method-error () nil))

     "BAZ Before all: abcd NIL. BAZ After ARRAY: abcd NIL. BAZ After STRING: abcd NIL. ")

    (test-error (baz "xyz") illegal-call-next-method-error)))
