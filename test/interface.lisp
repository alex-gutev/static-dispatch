;;;; test.lisp
;;;;
;;;; Copyright 2019-2020 Alexander Gutev
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

;;;; These tests test that the result of invoking a statically
;;;; dispatched generic function is correct. They do not test the
;;;; internal details of how a function is statically dispatched.
;;;;
;;;; NOTE: On some implementations, the generic functions invoked in
;;;; these tests might not be statically dispatched. In that case the
;;;; tests which check whether they were statically dispatched will
;;;; fail. In the event of the failure of these tests your program
;;;; will still function correctly however you'll likely be unable to
;;;; make use of statically dispatched generic functions and will
;;;; simply revert to the default standard dynamic dispatch.

;;;; These tests test the use of STATIC-DISPATCH from a high-level:
;;;;
;;;;  1. That Generic functions are actually statically dispatched
;;;;     when declared INLINE.
;;;;
;;;;  2. That the result of the generic function call is correct
;;;;     regardless of whether they are statically dispatched or not.
;;;;
;;;; NOTE:
;;;;
;;;;  If a test of the form "F is statically dispatched" fails, this
;;;;  means that STATIC-DISPATCH was unable to choose the appropriate
;;;;  method for F at compile-time, thus will fallback to dynamic
;;;;  dispatch. Programs using STATIC-DISPATCH will still function
;;;;  correctly, provided the remaining tests don't fail, however they
;;;;  will not be able to make use of statically dispatched generic
;;;;  functions on the implementation on which the test fails.

(defpackage :static-dispatch-interface-test
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:trivia

	:prove))

(in-package :static-dispatch-interface-test)

;;; Generic Function with Primary Methods

(defgeneric add (a b))

(defmethod add ((a number) (b number))
  (list 'number (+ a b)))

(defmethod add ((a string) (b string))
  (list 'string a b))

(defmethod add (a b)
  (list a b))


;;; Generic Function with Auxiliary Methods

(defgeneric my-eq (a b))

(defmethod my-eq ((a number) (b number))
  (= a b))

(defmethod my-eq (a b)
  (eq a b))


(defmethod my-eq :before ((a number) (b number))
  (format t "Before Numbers: ~a = ~a" a b))


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


;;; Tests

(defmacro test-dispatch (call result &key (test-dispatch t) (static-p t))
  (with-gensyms (static?-var)
    `(let ((,static?-var nil))
       (declare (ignorable ,static?-var))
       (macrolet ((static-dispatch::static-dispatch-test-hook ()
		    `(setf ,',static?-var t)))

	 ;; Suppress Output From Call
	 (is
	  (with-open-stream (*standard-output* (make-broadcast-stream)) ,call)
	  ,result)

	 ,(when test-dispatch
	    `(is ,static?-var ,static-p
		 ,(format nil "~a ~a dispatched"
			  call
			  (if static-p "statically" "dynamically"))))))))

(defconstant +a-constant+ 10)

(plan nil)

(subtest "Constant Arguments"
  (locally (declare (inline add))
    (test-dispatch (add 1 2) '(number 3))
    (test-dispatch (add "hello" "world") '(string "hello" "world"))
    (test-dispatch (add 'x 'y) '(x y) :test-dispatch nil)
    (test-dispatch (add +a-constant+ 1) '(number 11))))

(subtest "Variables with Type Declarations"
  (let ((x-int 1) (y-int 2) (z-int 3)
	(x-string "hello") (y-string "world")
	(x 'x) (y 'y))

    (declare (type number x-int y-int)
	     (type string x-string y-string)
	     (type (eql 3) z-int))
    (declare (inline add))

    (test-dispatch (add x-int y-int) '(number 3))
    (test-dispatch (add 1 x-int) '(number 2))
    (test-dispatch (add y-int 1/2) '(number 5/2))
    (test-dispatch (add x-int z-int) '(number 4))

    (test-dispatch (add x-string y-string) '(string "hello" "world"))
    (test-dispatch (add "hello" y-string) '(string "hello" "world"))
    (test-dispatch (add x-string "world") '(string "hello" "world"))

    (test-dispatch (add x y) '(x y) :test-dispatch nil)
    (test-dispatch (add x 1) '(x 1) :test-dispatch nil)
    (test-dispatch (add x-int x-string) '(1 "hello") :test-dispatch nil)))

(subtest "Functions with FTYPE Declarations"
  (flet ((neg (x)
	   (- x))

	 (half (x)
	   (floor x 2))

	 (reverse-string (str)
	   (reverse str))

	 (f (x) x))

    (declare (ftype (function (number) number) neg)
	     (ftype (function (number) (values number number)) half)
	     (ftype (function (string) string) reverse-string))
    (declare (inline add reverse-string))

    (let ((x 1) (y 2)
	  (hello "hello") (world "world"))
      (declare (type number x y)
	       (type string hello world))

      (test-dispatch (add (neg 3) 1) '(number -2))
      (test-dispatch (add y (neg x)) '(number 1))
      (test-dispatch (add (neg y) (neg 5)) '(number -7))
      (test-dispatch (add x (half y)) '(number 2))

      (test-dispatch (add (reverse-string "leh") "lo") '(string "hel" "lo"))
      (test-dispatch (add world (reverse-string hello)) '(string "world" "olleh"))
      (test-dispatch (add (reverse-string hello) (reverse-string world)) '(string "olleh" "dlrow"))

      (test-dispatch (add (f 'x) 'y) '(x y) :test-dispatch nil)
      (test-dispatch (add (neg 3) "x") '(-3 "x") :test-dispatch nil)
      (test-dispatch (add hello (neg 9)) '("hello" -9) :test-dispatch nil))))

(subtest "THE forms"
  (let ((x 5)
	(hello "hello"))
    (declare (inline add))

    (test-dispatch (add (the number (second (add 1 2)))
			(the number (f x)))
		   '(number 8))

    (test-dispatch (add (the integer (second (add 1 2)))
			1)
		   '(number 4))

    (test-dispatch (add 3
			(the fixnum (second (add 5 6))))
		   '(number 14))


    (test-dispatch (add (the string (second (add hello "")))
			"world")
		   '(string "hello" "world"))))

(subtest "Macros"
  (macrolet
      ((pass1 (x) x)
       (pass2 (y) y)

       (the-number (form) `(the number ,form))
       (the-string (form) `(the string ,form)))

    (flet ((neg (x) (- x)))
      (declare (ftype (function (number) number) neg))

      (let ((x 1) (y 2))
	(declare (type number x y))
	(declare (inline add))

	(symbol-macrolet ((x-mac x)
			  (number-mac (the number (second (add 1 2)))))

	  (test-dispatch (add (pass1 x) (pass2 y)) '(number 3))
	  (test-dispatch (add (pass1 y) (pass2 (neg x))) '(number 1))
	  (test-dispatch (add (pass1 (the-number (second (add x 1))))
			      (pass2 3))
			 '(number 5))

	  (test-dispatch (add x-mac 2) '(number 3))
	  (test-dispatch (add a-number number-mac) '(number 5))

	  (test-dispatch (add (pass-through (the-string (map 'string #'char-upcase "hello")))
			      (pass2 "world"))
			 '(string "HELLO" "world"))

	  (test-dispatch (add (pass-through 1)
			      (pass-through "world"))
			 '(1 "world")

			 :test-dispatch nil))))))


(subtest "Auxiliary Methods"
  ;; Suppress Output from :BEFORE method
  (locally (declare (inline my-eq))
    (test-dispatch (my-eq 1/2 0.5) t)
    (test-dispatch (my-eq 1 2) nil)
    (test-dispatch (my-eq "x" 'x) nil)

    (is-print (my-eq 1 2) "Before Numbers: 1 = 2")))

(subtest "Interaction with Other Compiler Macros"
  (isnt (compiler-macro-function 'f) #'static-dispatch
	"Compiler-Macro-Function of F not replaced by STATIC-DISPATCH")

  (locally (declare (inline f))
    (test-dispatch (f 1) 1 :static-p nil)
    (test-dispatch (f 'x) nil :static-p nil)))

(finalize)
