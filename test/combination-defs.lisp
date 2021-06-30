;;;; combination-defs.lisp
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

(defpackage :static-dispatch/test.combinations
  (:use :static-dispatch-cl
	:alexandria
	:arrows

	:fiveam
	:static-dispatch/test))

(in-package :static-dispatch/test.combinations)

;;; Method Combinations

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-method-combination my-list :operator list :identity-with-one-argument t))

;;;; Long-form method combination. Taken from generic-cl

(define-condition no-method-for-type (error)
  ((the-type :initarg :type
             :reader the-type))

  (:report
   (lambda (e s)
     (format s "No method for type: ~s." (the-type e)))))

;; ECL doesn't support the :ARGUMENTS keyword in define-method-combination.

#-ecl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-method-combination subtype ()
    ((methods * :required t))
    (:arguments type)

    "Dispatch based on a type keyword given as a qualifier.

     With this method combination each method includes a single
     qualifier, which is interpreted as a type specifier symbol.

     The method of which the value given in the first argument,
     interpreted as a type specifier, is a subtype of the method's
     qualified type specifier, given in the first qualifier, is
     called.

     The methods are ordered such that the methods qualified with the
     most derived type, i.e. a type which is a subtype of the others,
     are called first."

    (labels ((type-qualifier (method)
               (first (method-qualifiers method)))

             (type< (t1 t2)
               (subtypep t1 t2))

             (make-if (method else)
               `(if (subtypep ,type ',(type-qualifier method))
                    (call-method ,method)
                    ,else)))

      (-<> (copy-list methods)
           (stable-sort #'type< :key #'type-qualifier)
           (reduce #'make-if <>
                   :from-end t
                   :initial-value
                   `(error 'no-method-for-type :type ,type))))))


;;; Generic function with a standard method combination

(defgeneric foo-list (thing)
  (:method-combination list))

(defmethod foo-list list (thing)
  (list :default thing))

(defmethod foo-list list ((thing number))
  (list :number thing))

(defmethod foo-list list ((thing integer))
  (list :integer thing))

(defmethod foo-list :around ((thing integer))
  (if (= thing 3)
      (list :special 3)
      (list :around-integer (call-next-method))))

(defmethod foo-list list ((thing string))
  (list :string thing))


;;; Generic function with a user-defined short-form method
;;; combination

(defgeneric bar-list (thing)
  (:method-combination my-list))

(defmethod bar-list my-list (thing)
  (list :default thing))

(defmethod bar-list my-list ((thing number))
  (list :number thing))

(defmethod bar-list my-list ((thing integer))
  (list :integer thing))

(defmethod bar-list :around ((thing (eql 3)))
  (list :special 3))

(defmethod bar-list :around ((thing number))
  (list :around-number (call-next-method)))

(defmethod bar-list my-list ((thing string))
  (list :string thing))


;;; Generic function with a user-defined long-form method combination

#-ecl
(progn
 (defgeneric baz-subtype (type thing)
   (:method-combination subtype))

 (defmethod baz-subtype number (type thing)
            (list :number type thing))

 (defmethod baz-subtype array (type thing)
            (list :array type thing))

 (defmethod baz-subtype integer (type thing)
            (list :integer type thing)))
