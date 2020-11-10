;;;; test.lisp
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

;;;; Unit Tests

(defpackage :static-dispatch-test
  (:use :common-lisp
	:alexandria
	:arrows
	:trivia

	:prove)

  (:import-from
   :static-dispatch
   :parse-method

   :*generic-function-table*
   :gf-method
   :gf-methods

   :method-info
   :body
   :lambda-list
   :specializers

   :precedence-order
   :order-by-precedence
   :order-method-specializers
   :applicable-methods
   :sort-methods
   :specializer<

   :*current-gf*
   :inline-method-body
   :block-name))

(in-package :static-dispatch-test)

(plan nil)

(subtest "DEFMETHOD form parser"
  ;; Test parse-method function

  ;; Simple method with only required parameters and class specializers
  (is-values (parse-method
	      '(((a number) b (c character) (d t))
		(declare (ignore d))
		(pprint c)
		(+ a b)))

	     '((number t character t)
	       (a b c d)
	       ((declare (ignore d))
		(pprint c)
		(+ a b))))

  ;; Same method with an EQL specializer

  (is-values (parse-method
	      '(((a number) b (c character) (d t) (e (eql 3)))
		(declare (ignore d))
		(pprint c)
		(+ a b)))

	     '((number t character t (eql 3))
	       (a b c d e)
	       ((declare (ignore d))
		(pprint c)
		(+ a b))))

  ;; Other argument types

  (let ((required '((a number) b (c character) (d t) (e (eql 3))))
	(other-args '(&optional x (y 1) &rest args &key x (y 'y y-p) (z)))
	(body '((declare (ignore d))
		(pprint c)
		(+ a b))))

    (is-values (parse-method `((,@required ,@other-args) ,@body))
	       `((number t character t (eql 3))
		 (a b c d e ,@other-args)
		 ,body)))

  ;; Qualifiers - Should raise a MATCH-ERROR as qualifiers are not supported yet.

  (is-error (parse-method '(:before (a (b number) (c character)) (pprint c) (+ a b)))
	    'trivia:match-error)

  (is-error (parse-method '(and (a (b number) (c character)) (pprint c) (+ a b)))
	    'trivia:match-error))

;; Generic function used in testing the DEFMETHOD macro.

(defgeneric equal? (a b))

(subtest "DEFMETHOD macro"
  ;; Test that that the DEFMETHOD macro is adding the method
  ;; information to the generic function method table.

  (let ((*generic-function-table* (make-hash-table :test #'eq)))
    (let* ((method `(static-dispatch:defmethod equal? ((a number) (b number))
		      (= a b))))

      (macroexpand method)

      ;; Check that a method table was created for EQUAL?
      (ok (gf-methods 'equal?))

      (let ((method-info (gf-method 'equal? '(number number))))
	;; Check that the method was added to the method table for
	;; EQUAL?

	(ok method-info)
	(is (lambda-list method-info) '(a b))
	(is (specializers method-info) '(number number))
	(is (body method-info) (cdddr method))))

    (let* ((method `(static-dispatch:defmethod equal? (a b)
		      (eq a b))))

      (macroexpand method)

      ;; Check that the (NUMBER NUMBER) method is still in the method
      ;; table
      (ok (gf-method 'equal? '(number number)))

      (let ((method-info (gf-method 'equal? '(t t))))
	;; Check that the method was added to the method table for
	;; EQUAL?

	(ok method-info)
	(is (lambda-list method-info) '(a b))
	(is (specializers method-info) '(t t))
	(is (body method-info) (cdddr method))))

    (let* ((method `(static-dispatch:defmethod equal? ((x (eql 'all)) (y t)) t)))

      ;; Evaluate DEFMETHOD form to actually create the method and add
      ;; a method for (EQL ALL) to the generic function table
      (eval method)

      (let ((method-info (gf-method 'equal? '((eql all) t))))
	;; Check that the method was added to the method table for
	;; EQUAL?

	(ok method-info)
	(is (lambda-list method-info) '(x y))
	(is (specializers method-info) '((eql all) t))
	(is (body method-info) (cdddr method))))

    ;; Test Qualifiers

    (macroexpand '(static-dispatch:defmethod equal? :before (x y)
		   (pprint x)
		   (pprint y)))



    ;; Check that the method table for equal? was removed as
    ;; qualifiers are not supported.

    (is (gf-methods 'equal?) nil)

    ;; Check that even if a new method with no qualifiers is defined,
    ;; the method table will not be recreated.

    (macroexpand '(static-dispatch:defmethod :before (x y)
		   (pprint x)
		   (pprint y)))

    (is (gf-methods 'equal?) nil)))


;; Classes used to test method ordering

(c2mop:defclass person ()
  (name age))

(c2mop:defclass child (person)
  (parent))


(subtest "Method Ordering"
  ;; Tests that the methods are being ordered correctly based on
  ;; specificity.

  (subtest "Specializer Ordering"
    ;; Test SPECIALIZE< function

    ;; Test ordering based on class precedence
    (ok (specializer< '(child t) '(person number)))
    (is (specializer< '(person number) '(child t)) nil)

    ;; Test ordering when first argument specializers are identical
    (ok (specializer< '(child number) '(child t)))
    (is (specializer< '(child t) '(child number)) nil)

    ;; Test ordering when initial specializers are unrelated classes.
    (ok (specializer< '(child number) '(character t)))
    (is (specializer< '(character t) '(child number)) nil)

    ;; Test ordering when all specializers are unrelated classes. In
    ;; this case the specializers should be ordered based on the
    ;; orderings of the names of the classes of the last
    ;; specializers. This is to ensure that there is a total ordering
    ;; of class specializers.
    (ok (specializer< '(child number) '(character string)))
    (is (specializer< '(character string) '(child number)) nil)

    ;; Test EQL specializers
    (ok (specializer< '((eql all) t) '(child number)))
    (is (specializer< '(child number) '((eql all) t)) nil)

    ;; Test ordering of identical EQL specializers
    (ok (specializer< '((eql all) number) '((eql all) t)))
    (is (specializer< '((eql all) t) '((eql all) number)) nil)

    ;; Test ordering of different EQL specializers
    (is (specializer< '((eql 1) number) '((eql 2) t)) nil))

  (let* ((methods
	  (mapcar
	   #'list
	   '((number t)
	     (character character)
	     (t t)
	     (person person)
	     (integer t)
	     (t (eql 1))
	     ((eql x) t)
	     ((eql x) string)
	     (person child)))))

    (subtest "Method ordering based on specializer ordering"
      ;; Test SORT-METHODS function

      (is (sort-methods (copy-list methods))
	  (mapcar
	   #'list
	   '(((eql x) string)
	     ((eql x) t)

	     (character character)
	     (person child)
	     (person person)
	     (integer t)
	     (number t)

	     (t (eql 1))
	     (t t)))))

    (subtest "Determining the applicable methods"
      ;; Test APPLICABLE-METHODS function

      (is (applicable-methods methods '(integer integer))
	  (mapcar #'list '((number t) (t t) (integer t))))

      (is (applicable-methods methods '(number number))
	  (mapcar #'list '((number t) (t t))))

      (is (applicable-methods methods '(number t))
	  (mapcar #'list '((number t) (t t))))

      (is (applicable-methods methods '(child child))
	  (mapcar #'list '((t t) (person person) (person child))))

      (is (applicable-methods methods '(child person))
	  (mapcar #'list '((t t) (person person))))

      (is (applicable-methods methods '((eql x) number))
	  (mapcar #'list '((t t) ((eql x) t))))

      ;; Check that if there is not enough type information for an
      ;; argument and not all the specializers for that argument, of
      ;; all methods, are T then APPLICABLE-METHODS returns nil.
      (is (applicable-methods methods '(t t)) nil))))


(subtest "Ordering by generic function argument precedence order"
  ;; Test PRECEDENCE-ORDER function

  (is (precedence-order '(a b c &optional d e) '(c a b)) '(2 0 1))

  ;; Test ORDER-BY-PRECEDENCE function

  (is (order-by-precedence '(2 0 1) '(x (+ a b) 1 "optional arg")) '(1 x (+ a b)))

  ;; Test ORDER-METHOD-SPECIALIZERS function

  (is (order-method-specializers
       (mapcar
	#'list
	'((number t character)
	  (integer t t)
	  (t t t)))
       '(2 0 1))
      (mapcar
       #'list
       '((character number t)
	 (t integer t)
	 (t t t)))))

(subtest "Method inlining"
  ;; Test the BLOCK-NAME function

  (is (block-name 'equal?) 'equal?)
  (is (block-name '(setf field)) 'field)

  ;; Test the output of the INLINE-METHOD-BODY function

  (let ((*check-types*)
	(*current-gf* 'equal?)
	(method1 (make-instance 'method-info
				:body '(= a b)
				:lambda-list '(a b &optional c)
				:specializers '(number number)))
	(method2 (make-instance 'method-info
				:body '(eq x y)
				:lambda-list '(x y &optional c)
				:specializers '(t t))))
    (declare (special *check-types*))

    (block inline-method-test
      (macrolet ((fail-test (desc)
		   "Fail test with description DESC and skip remaining
                    tests."

		   `(progn
		      (fail ,desc)
		      (return-from inline-method-test nil))))

	(labels ((test-inline-method (args method next-methods &optional *check-types* types)
		   "Test the result of inlining METHOD, with next
                    methods NEXT-METHODS and arguments ARGS."

		   (declare (special *check-types*))

		   (-> (inline-method-body method args next-methods *check-types* types)
		       (test-inline-form args method next-methods types)))

		 (test-inline-form (form args method next-methods &optional types)
		   "Test whether the inline method form, FORM, is
                    correct for METHOD, next methods NEXT-METHODS and
                    arguments ARGS."

		   (match form
		     ((list 'static-dispatch-cl:flet (list* fns)
			    '(static-dispatch-cl:declare (ignorable #'static-dispatch-cl:call-next-method #'static-dispatch-cl:next-method-p))
			    body)
		      (let ((args (if (listp args) `(list ,@args) args)))
			(test-flet-fns fns args next-methods)
			(test-flet-body body args method types)

			(pass "Correct inline method form.")))

		     (_
		      (fail-test (format nil "Incorrect inline method form: ~s." form)))))

		 (test-flet-fns (fns args next-methods)
		   "Tests whether the lexical function definitions,
                    FNS, for CALL-NEXT-METHOD and NEXT-METHOD-P are
                    correct."

		   (match fns
		     ((list
		       (list* 'static-dispatch-cl:call-next-method next-fn)
		       (list* 'static-dispatch-cl:next-method-p next-p-fn))

		      (test-call-next-method next-fn args next-methods)
		      (test-next-method-p next-p-fn next-methods)

		      (pass "FLET definitions for CALL-NEXT-METHOD and NEXT-METHOD-p correct."))

		     (_
		      (fail-test (format nil "FLET definitions for CALL-NEXT-METHOD and NEXT-METHOD-P incorrect: ~s." fns)))))


		 ;; FLET Body

		 (test-flet-body (body args method types)
		   "Tests whether the body of the FLET form,
                    containing the actual inline method body, is
                    correct."

		   (with-slots (lambda-list specializers) method
		     (let ((req-args (subseq lambda-list 0 (length specializers))))
		      (match body
			((list 'static-dispatch-cl:block (equal (block-name *current-gf*))
			       (list* 'static-dispatch-cl:destructuring-bind
				      (equal lambda-list)
				      (equal args)
				      (list 'static-dispatch-cl:declare
					    (list* 'ignorable
						   (equal req-args)))
				      body))

			 (test-method-body body (listp args) method types)

			 (pass "Body of inline FLET form is correct."))

			(_
			 (fail-test (format nil "Body of inline FLET form is incorrect: ~s." body)))))))

		 (test-method-body (body decl-p method types)
		   "Tests whether the inline method body matches the
                    body of METHOD. If DECL-P is true then local type
                    declarations for the arguments are expected."

		   (with-slots (lambda-list specializers) method

		    (match body
		      ((guard
			(list* (list* 'static-dispatch-cl:declare declarations)
			       (equal (body method)))
			decl-p)

		       ;; Test declarations

		       (if (equal declarations (mapcar (curry #'list 'type) types lambda-list))
			   (pass "Method argument type declarations correct.")
			   (fail-test (format nil "Incorrect method argument type declarations: ~s." declarations)))

		       (pass "Inline method body matches method body."))

		      (body

		       (or
			(if *check-types*
			    (equal (test-check-type body lambda-list specializers) (body method))
			    (equal body (body method)))
			(trivia.fail:fail))

		       (pass "Inline method body matches method body."))

		      (_
		       (fail-test (format nil "Inline BODY does not match method body: ~s." body))))))

		 (test-check-type (body args types)
		   "Check whether the first few forms of BODY are
                    CHECK-TYPE forms for the variables in ARGS and the
                    corresponding types in TYPES."

		   (if types
		       (match body
			 ((list* (list 'static-dispatch-cl:check-type
				       (eql (first args))
				       (equal (first types)))
				 body)

			  (test-check-type body (rest args) (rest types)))

			 (_ (fail-test (format nil "Incorrect type checks in method body: ~s." body))))
		       body))


		 ;; Test NEXT-METHOD-P

		 (test-next-method-p (fn next-methods)
		   "Tests whether the NEXT-METHOD-P function
                    definition is correct."

		   (match fn
		     ((list nil (eql (and next-methods t)))
		      (pass "NEXT-METHOD-P definition is correct."))

		     (_
		      (fail-test (format nil "Incorrect NEXT-METHOD-P definition: ~s." fn)))))


		 ;; Test CALL-NEXT-METHOD

		 (test-call-next-method (fn args next-methods)
		   "Tests whether the CALL-NEXT-METHOD function
                    definition is correct."

		   (match fn
		     ((list
		       (list '&rest fn-arg)
		       body)

		      (test-next-method-body body fn-arg args next-methods)
		      (pass "CALL-NEXT-METHOD definition is correct."))

		     (_
		      (fail-test (format nil "Incorrect CALL-NEXT-METHOD definition: ~s." fn)))))

		 (test-next-method-body (body fn-arg args next-methods)
		   "Tests whether the body of the CALL-NEXT-METHOD
                    function is correct."

		   (match body
		     ((list
		       'static-dispatch-cl:let
		       (list (list next-args next-args-init))
		       body)

		      (test-next-args-initform next-args-init fn-arg args)

		      (if next-methods
			  (test-inline-form body next-args (first next-methods) (rest next-methods))
			  (test-no-next-method body next-args))

		      (pass "Body of CALL-NEXT-METHOD is correct."))

		     (_ (fail-test (format nil "Incorrect CALL-NEXT-METHOD body: ~s" body)))))

		 (test-next-args-initform (form fn-arg args)
		   "Tests whether the init-form of the variable,
                    storing the arguments passed to the next method,
                    is correct."

		   (match form
		     ((list 'static-dispatch-cl:or (eql fn-arg) (equal args))
		      (pass "Next method arguments are correct."))

		     (_ (fail-test (format nil "Incorrect next method arguments: ~s." form)))))

		 (test-no-next-method (body args)
		   "Tests whether the call to NO-NEXT-METHOD is correct."

		   (if (equal `(apply #'static-dispatch-cl:no-next-method ',*current-gf* nil ,args) body)
		       (pass "Call to NO-NEXT-METHOD is correct.")
		       (fail-test (format nil "Incorrect NO-NEXT-METHOD call: ~s." body)))))

	  (test-inline-method '((+ u v) 3) method1 (list method2) nil '(integer integer))
	  (test-inline-method '(y z) method2 nil t)

	  ;; Test with type-checks
	  (test-inline-method '((+ u v) 3) method1 (list method2) t '(number number))

	  ;; Test SETF methods

	  (let ((*current-gf* '(setf field)))
	    (test-inline-method '(a b) method2 nil nil '(t t))))))))

(finalize)
