;;;; util.lisp
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

;;;; Utilities for testing static-dispatch

(defpackage :static-dispatch-test-util
  (:use :static-dispatch-cl
	:alexandria
	:arrows
	:trivia

	:prove)

  (:export :test-dispatch
	   :suppress-output
	   :test-error))

(in-package :static-dispatch-test-util)

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

(defmacro suppress-output (&body forms)
  `(with-open-stream (*standard-output* (make-broadcast-stream))
     ,@forms))

(defmacro test-error (form error)
  `(is-error (suppress-output ,form) ,error))
