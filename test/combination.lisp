;;;; combination.lisp
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

;;;; Test generic functions with method combinations other than
;;;; standard.
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

(in-package :static-dispatch/test.combinations)


;;; Test suite definition

(def-suite method-combinations
    :description "Test method combinations"
    :in static-dispatch)

(in-suite method-combinations)


;;; Tests

(test (standard-list-combination :compile-at :run-time)
  "Test another method combination provided by the CL standard"

  (locally
      (declare (optimize (speed 3) (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))

    (test-dispatch (foo-list 2) '(:around-integer ((:integer 2) (:number 2) (:default 2))))
    (test-dispatch (foo-list 3) '(:special 3))
    (test-dispatch (foo-list 1/2) '((:number 1/2) (:default 1/2)))
    (test-dispatch (foo-list "hello") '((:string "hello") (:default "hello")))
    (test-dispatch (foo-list 'x) '((:default x)))))

(test (short-form-combination :compile-at :run-time)
  "Test method-combination defined with short-form definition"

  (locally
      (declare (optimize (speed 3) (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))

    (test-dispatch (bar-list 2) '(:around-number ((:integer 2) (:number 2) (:default 2))))
    (test-dispatch (bar-list 3) '(:special 3))
    (test-dispatch (bar-list 1/2) '(:around-number ((:number 1/2) (:default 1/2))))
    (test-dispatch (bar-list "hello") '((:string "hello") (:default "hello")))
    (test-dispatch (bar-list 'x) '(:default x))))

(test (long-form-combination :compile-at :run-time)
  "Test method-combination defined with long-form definition"

  (locally
      (declare (optimize (speed 3) (safety 1) (debug 1) #+sbcl sb-ext:inhibit-warnings))

    (test-dispatch (baz-subtype 'fixnum 10) '(:integer fixnum 10))
    (test-dispatch (baz-subtype 'ratio 3/5) '(:number ratio 3/5))
    (test-dispatch (baz-subtype 'string "abc") '(:array string "abc"))

    (test-dispatch
     (handler-case (baz-subtype 'symbol 'symb)
       (no-method-for-type () 'cant-process-symbol))

     'cant-process-symbol)))
