;;;; package.lisp
;;;;
;;;; Copyright 2018-2019 Alexander Gutev
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

(uiop:define-package :closer-environments
    (:use)

  (:mix
   :closer-mop
   :cl-environments)

  (:reexport :cl-environments
             :closer-mop))

(defpackage #:static-dispatch
  (:use :alexandria
	:anaphora
	:arrows
	:optima
	:iterate

	:agutil
	:closer-environments
        :cl-form-types)

  (:shadow :defmethod
	   :defgeneric
           :define-method-combination

           :call-method
           :make-method
           :invalid-method-error
           :method-combination-error)

  (:export :defmethod
	   :defgeneric
           :define-method-combination

           :call-method
           :make-method
           :invalid-method-error
           :method-combination-error

	   :enable-static-dispatch
	   :static-dispatch

           :static-dispatch-warn
           :static-dispatch-inhibit
           :static-dispatch-type))

(uiop:define-package :static-dispatch-cl
    (:use)

  (:mix
   :static-dispatch
   :closer-environments)

  (:reexport :static-dispatch
             :closer-environments))
