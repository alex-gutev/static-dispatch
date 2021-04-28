;;;; test.lisp
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

;;;; Master test suite

(defpackage :static-dispatch/test
  (:use :static-dispatch-cl
	:fiveam)

  (:import-from :alexandria
		:with-gensyms)

  (:export :static-dispatch
	   :test-dispatch
	   :suppress-output
	   :test-error
	   :is-print))

(in-package :static-dispatch/test)

(def-suite static-dispatch
    :description "Static Dispatch Tests")

(in-suite static-dispatch)

(defun test-static-dispatch ()
  (run! 'static-dispatch))


;;; Utilities

(defvar *static-dispatch*)

(defmacro static-dispatch::static-dispatch-test-hook ()
  '(setf *static-dispatch* t))

(defmacro test-dispatch (call result &key (test-dispatch t) (static-p t))
  `(let ((*static-dispatch* nil))
     (is
      (equal
       ,result
       (suppress-output ,call)))

     ,(when test-dispatch
	`(,(if static-p 'is-true 'is-false)
	   *static-dispatch*
	   ,(format nil "~a ~a dispatched"
		    call
		    (if static-p "statically" "dynamically"))))))

(defmacro suppress-output (&body forms)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     ,@forms))

(defmacro test-error (form error)
  `(signals ,error (suppress-output ,form)))

(defmacro is-print (form string)
  "Test that the evaluation of FORM results in STRING being printed to
  *STANDARD-OUTPUT*"

  (with-gensyms (output expected)
    `(let ((,output
	    (with-output-to-string (*standard-output*)
	      ,form))
	   (,expected ,string))

       (is (string= ,expected ,output)
	   "Expected ~a to print:~%~a~%Got:~%~a"
	   ,expected
	   ,output))))
