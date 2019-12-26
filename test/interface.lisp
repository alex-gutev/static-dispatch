;;;; test.lisp
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

(in-package :static-dispatch-interface-test)

(defmacro test-dispatch (call result &key (test-dispatch t) (static-p t))
  (with-gensyms (static?-var)
    `(let ((,static?-var nil))
       (declare (ignorable ,static?-var))
       (macrolet ((static-dispatch::static-dispatch-test-hook ()
		    `(setf ,',static?-var t)))
	 (is ,call ,result)
	 ,(when test-dispatch
	    `(is ,static?-var ,static-p
		 ,(format nil "~a ~a dispatched"
			  call
			  (if static-p "statically" "dynamically"))))))))

(plan nil)

(subtest "Constant Arguments"
  (locally (declare (inline add))
    (test-dispatch (add 1 2) '(number 3))
    (test-dispatch (add "hello" "world") '(string "hello" "world"))
    (test-dispatch (add 'x 'y) '(x y) :test-dispatch nil)))

(subtest "Variables with Type Declarations"
  (let ((x-int 1) (y-int 2)
	(x-string "hello") (y-string "world")
	(x 'x) (y 'y))
    (declare (type number x-int y-int)
	     (type string x-string y-string))
    (declare (inline add))

    (test-dispatch (add x-int y-int) '(number 3))
    (test-dispatch (add 1 x-int) '(number 2))
    (test-dispatch (add y-int 1/2) '(number 5/2))

    (test-dispatch (add x-string y-string) '(string "hello" "world"))
    (test-dispatch (add "hello" y-string) '(string "hello" "world"))
    (test-dispatch (add x-string "world") '(string "hello" "world"))

    (test-dispatch (add x y) '(x y) :test-dispatch nil)
    (test-dispatch (add x 1) '(x 1) :test-dispatch nil)
    (test-dispatch (add x-int x-string) '(1 "hello") :test-dispatch nil)))


(subtest "Interaction with Other Compiler Macros"
  (isnt (compiler-macro-function 'f) #'static-dispatch
	"Compiler-Macro-Function of F not replaced by STATIC-DISPATCH")

  (locally (declare (inline f))
    (test-dispatch (f 1) 1 :static-p nil)
    (test-dispatch (f 'x) nil :static-p nil)))

(finalize)
