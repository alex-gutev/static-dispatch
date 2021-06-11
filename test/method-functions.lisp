;;;; method-functions.lisp
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
;;;; This test suite tests static overloading, rather than inline. In
;;;; static overloading the generic function call is replaced directly
;;;; with a call to the applicable method, rather than with its body.
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

(in-package :static-dispatch/test.method-functions)


;;; Test suite definition

(def-suite method-functions
    :description "Test replacing generic function call with method function call"
    :in static-dispatch)

(in-suite method-functions)


;;; Tests

;; Enable static dispatch with overloading rather than inlining

(enable-static-dispatch
 (:function foo)
 (:function my-eq)
 (:function foo2)
 (:function bar)
 (:function baz))

;; Inhibit notes on SBCL
#+sbcl (declaim (optimize sb-ext:inhibit-warnings))

(test (dispatch-integer-arguments :compile-at :run-time)
  "Test CALL-NEXT-METHOD with arguments"

  (let ((x-int 5) (y-int 10))
    (declare (type integer x-int y-int)
	     (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings)
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

(test (dispatch-float-arguments :compile-at :run-time)
  "Test CALL-NEXT-METHOD without arguments"

  (let ((x 0.5) (y 2.5))
    (declare (type float x y)
	     (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings)
	     (inline foo))

    (test-dispatch
     (foo (pass-through 1.5) 3.125)
     '(float t (number t (1.5 3.125))))

    (test-dispatch
     (foo x y)
     '(float t (number t (0.5 2.5))))))

(test (dispatch-number-arguments :compile-at :run-time)
  "Test NEXT-METHOD-P with next method"

  (let ((x 1) (y 3/2))
    (declare (type integer x)
	     (type number y)
	     (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings)
	     (inline foo))

    (test-dispatch
     (foo 1 5/6)
     '(number t (1 5/6)))

    (test-dispatch
     (foo x y)
     '(number t (1 3/2)))))

(test (dispatch-string-arguments :compile-at :run-time)
  "Test NEXT-METHOD-P with next method."

  (let ((hello "hello"))
    (declare (type string hello)
	     (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings)
	     (inline foo))

    (test-dispatch
     (foo hello "world")
     '(string t ("hello" "world")))

    (test-dispatch
     (foo (pass-through "hello") "bye")
     '(string t ("hello" "bye")))))

(test (dispatch-other-arguments :compile-at :run-time)
  "Test NEXT-METHOD-P with no next method"

  (locally (declare (inline foo)
		    (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))

    (test-dispatch (foo 'x 0) '(other nil (x 0)))
    (test-dispatch (foo (pass-through "hello") +a-constant+)
		   (list 'other nil (list "hello" +a-constant+)))

    (test-dispatch (foo 'x 'y :default-type 'default)
		   (list 'default nil (list 'x 'y)))))


;;; Test Auxiliary methods

(test (around-methods :compile-at :run-time)
  "Test static dispatching of :AROUND methods"

  (locally (declare (inline my-eq)
		    (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))

    (test-dispatch (my-eq 1/2 0.5) '(:around-number t))
    (test-dispatch (my-eq 1 2) '(:around-integer (:around-number nil)))
    (test-dispatch (my-eq "x" 'x) nil)
    (test-dispatch (my-eq 133 133) :special-number)))

(test (before-after-methods :compile-at :run-time)
  "Test static dispatching of :BEFORE and :AFTER methods"

  (locally (declare (inline my-eq)
		    (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))

    (is-print (my-eq 1/2 2/3) "Before Numbers: 1/2 = 2/3. After Numbers: 1/2 = 2/3. ")
    (is-print (my-eq 1 2) "Before Integer: 1 = 2. Before Numbers: 1 = 2. After Numbers: 1 = 2. After Integer: 1 = 2. ")

    ;; Test that :BEFORE and :AFTER methods are not called when not
    ;; applicable.
    (is-print (my-eq 'x 'y) "")))

(test (before-method-without-primary :compile-at :run-time)
  "Test static dispatching of :BEFORE method without primary method"

  (locally (declare (inline foo2) (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))
    (is-print
     (handler-case (foo2 "x")
       (error () nil))
     "")

    (test-error (foo2 1) error)))

(test (after-method-without-primary :compile-at :run-time)
  "Test static dispatching of :AFTER method without primary method"

  (locally (declare (inline bar) (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))
    (is-print
     (handler-case (bar 1)
       (error () nil))
     "")

    (test-error (bar 5) error)))

(test (around-method-without-primary :compile-at :run-time)
  "Test static dispatching of :AROUND method without primary method"

  (locally (declare (inline bar) (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))
    (test-error (bar "hello") error)
    (test-error (bar 10) error)))

(test (before-method-call-next-method :compile-at :run-time)
  "Test CALL-NEXT-METHOD from :BEFORE method"

  (locally (declare (inline baz) (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))
    (is-print (baz 'x) "BAZ Before all: X NIL. ")

    (is-print
     (handler-case
	 (baz 1)
       (error () nil))
     "BAZ Before INTEGER: 1 NIL. ")

    (test-error (baz 14) error)))

(test (after-method-call-next-method :compile-at :run-time)
  "Test CALL-NEXT-METHOD from :AFTER method"

  (locally (declare (inline baz) (optimize speed (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))
    (is-print (baz #(1 2 3))
	      "BAZ Before all: #(1 2 3) NIL. BAZ After ARRAY: #(1 2 3) NIL. ")

    (is-print
     (handler-case
	 (baz "abcd")
       (error () nil))

     "BAZ Before all: abcd NIL. BAZ After ARRAY: abcd NIL. BAZ After STRING: abcd NIL. ")

    (test-error (baz "xyz") error)))
