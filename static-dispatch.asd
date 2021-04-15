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
  :version "0.3"
  :serial t
  :components ((:module
		"src"
		:components
		((:file "package")
		 (:file "common")
		 (:file "default" :if-feature (:not :sbcl))
		 (:file "sbcl" :if-feature :sbcl))))

  :depends-on (:alexandria
	       :anaphora
	       :arrows
	       :iterate
	       :trivia
	       :closer-mop

	       :agutil
	       :cl-environments)

  :in-order-to ((asdf:test-op (asdf:test-op :static-dispatch/test))))

(asdf:defsystem #:static-dispatch/test
  :description "Tests for static-dispatch."
  :author "Alexander Gutev"
  :license "MIT"
  :depends-on (:static-dispatch :cl-interpol :prove :prove-asdf)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module
		"test"
		:components
		((:file "util")
		 (:test-file "internals")
		 (:test-file "dispatch")
		 (:test-file "next-methods")
		 (:test-file "aux")
		 (:test-file "compiler-macro"))))
  :perform (asdf:test-op :after (op c)
			 (funcall (intern #.(string :run) :prove) c :reporter :fiveam)))
