;;;; static-dispatch.asd
;;;;
;;;; Copyright 2018 Alexander Gutev
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

(asdf:defsystem #:static-dispatch
  :description "Static generic function dispatch for Common Lisp."
  :author "Alexander Gutev"
  :license "MIT"
  :version "0.7"
  :serial t
  :components ((:module
		"src"
		:components
		((:file "package")
		 (:file "util")
		 (:file "common")
		 (:file "method-functions")
		 (:file "default" :if-feature (:not :sbcl))
		 (:file "sbcl" :if-feature :sbcl))))

  :depends-on (:alexandria
	       :anaphora
	       :arrows
	       :iterate
	       :optima
	       :closer-mop

	       :agutil
	       :cl-environments
               :cl-form-types)

  :in-order-to ((asdf:test-op (asdf:test-op :static-dispatch/test))))

(asdf:defsystem #:static-dispatch/test
  :description "Tests for static-dispatch."
  :author "Alexander Gutev"
  :license "MIT"
  :depends-on (:static-dispatch :fiveam)
  :serial t
  :components ((:module
		"test"
		:serial t
		:components
		((:file "test")
		 (:file "internals")
		 (:file "dispatch-defs")
		 (:file "dispatch")
		 (:file "next-methods-defs")
		 (:file "next-methods")
		 (:file "aux-defs")
		 (:file "aux")
		 (:file "compiler-macro")
		 (:file "method-functions-defs")
		 (:file "method-functions"))))

  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :static-dispatch/test :test-static-dispatch)))
