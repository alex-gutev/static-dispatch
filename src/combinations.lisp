;;;; combin.lisp
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

;;;; Standard Method Combination Functions

(in-package #:static-dispatch)

(define-method-combination% standard ()
  ((around (:around))
   (before (:before))
   (primary () :required t)
   (after (:after) :order :most-specific-last))

  (labels ((call-methods (methods)
             (mapcar (curry #'list 'call-method) methods))

           (call-primary (primary)
             `(call-method ,(first primary) ,(rest primary)))

           (call-before (before form)
             (if before
                 `(progn
                    ,@(call-methods before)
                    ,form)
                 form))

           (call-after (after form)
             (if after
                 `(multiple-value-prog1 ,form
                    ,@(call-methods after))
                 form))

           (call-around (around form)
             (if around
                 `(call-method
                   ,(first around)
                   (,@(rest around)
                      (make-method ,form)))
                 form)))

    (->> (call-primary primary)
         (call-before before)
         (call-after after)
         (call-around around))))

(define-method-combination% + :operator + :identity-with-one-argument t)
(define-method-combination% append :operator append :identity-with-one-argument t)
(define-method-combination% max :operator max :identity-with-one-argument t)
(define-method-combination% nconc :operator nconc :identity-with-one-argument t)
(define-method-combination% progn :operator progn :identity-with-one-argument t)
(define-method-combination% and :operator and :identity-with-one-argument t)
(define-method-combination% list :operator list :identity-with-one-argument t)
(define-method-combination% min :operator min :identity-with-one-argument t)
(define-method-combination% or :operator or :identity-with-one-argument t)
