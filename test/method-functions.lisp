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
 (:overload foo)
 (:overload my-eq))



;; Inhibit notes on SBCL
#+sbcl (declaim (optimize sb-ext:inhibit-warnings))

(test (dispatch-integer-arguments :compile-at :run-time)
  "Test CALL-NEXT-METHOD with arguments"

  (let ((x-int 5) (y-int 10))
    (declare (type integer x-int y-int)
	     (optimize speed #+sbcl sb-ext:inhibit-warnings)
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
	     (optimize speed #+sbcl sb-ext:inhibit-warnings)
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
	     (optimize speed #+sbcl sb-ext:inhibit-warnings)
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
	     (optimize speed #+sbcl sb-ext:inhibit-warnings)
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
		    (optimize speed #+sbcl sb-ext:inhibit-warnings))

    (test-dispatch (foo 'x 0) '(other nil (x 0)))
    (test-dispatch (foo (pass-through "hello") +a-constant+)
		   (list 'other nil (list "hello" +a-constant+)))))
