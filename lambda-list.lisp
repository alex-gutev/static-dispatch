;;;; lambda-list.lisp
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

(in-package :static-dispatch)

(defclass lambda-list ()
  ((required :initform nil
	     :accessor required)
   (optional :initform nil
	     :accessor optional)
   (keyword :initform nil
	    :accessor keyword)
   (allow-other-keys :initform nil
		     :accessor allow-other-keys)
   (rest :initform nil
	 :accessor rest-arg)
   (aux :initform nil
	:accessor aux)))

(defclass generic-lambda-list (lambda-list)
  ((specializers :initform nil
		 :accessor specializers)))

(defun parse-generic-lambda-list (args)
  (flet ((parse-arg-opts (opts)
	   (match opts
	     ((or (list initform arg-sp)
		  (list initform)
		  nil)
	      (list initform arg-sp)))))

    (let ((lambda-list (make-instance 'generic-lambda-list)))
      (with-slots (specializers required optional keyword aux) lambda-list
	(setf specializers (make-queue)
	      required (make-queue)
	      optional (make-queue)
	      keyword (make-queue)
	      aux (make-queue))

	(match-state args
	  :start 'required

	  (&optional
	   (cons '&optional rest)
	   :from required

	   (next rest))

	  (&key
	   (cons '&key rest)
	   :from (required optional)

	   (next rest))

	  (&allow-other-keys
	   (cons '&allow-other-keys rest)
	   :from (&key keyword)

	   (setf (allow-other-keys lambda-list) t)
	   (next rest))

	  (&rest
	   (cons '&rest rest)
	   :from (required keyword &allow-other-keys)

	   (next rest))

	  (&aux
	   (cons '&aux rest)
	   :from (required keyword &allow-other-keys &rest rest)

	   (next rest))

	  (required
	   (cons (list arg specializer) rest) ;; TODO: Add guard on arg
	   :from required

	   (enqueue arg required)
	   (enqueue specializer specializers)
	   (next rest))

	  (required
	   (cons arg rest) ;; TODO: Add guard on arg
	   :from required

	   (enqueue arg required)
	   (enqueue t specializers)
	   (next rest))

	  (optional
	   (cons
	    (or (cons arg opts) arg) ;; TODO: Add guard on arg
	    rest)
	   :from (&optional optional)

	   (enqueue (cons arg (parse-arg-opts opts)) optional)
	   (next rest))

	  (keyword
	   (cons
	    (or (cons (or (list key arg) arg) opts)
		arg)
	    rest)
	   :from (&key keyword)

	   (enqueue (list* arg key (parse-arg-opts opts)) keyword)
	   (next rest))

	  (rest
	   (cons arg rest)
	   :from &rest

	   (setf (rest-arg lambda-list) arg)
	   (next rest))

	  (aux
	   (cons (or (cons var init) var) rest)
	   :from (&aux aux)

	   (enqueue (cons var init) aux)
	   (next rest))

	  (end-of-list nil)

	  (error lst
		 (error "Malformed lambda list: ~s" lst)))

	(setf specializers (queue->list specializers))
	(setf required (queue->list required))
	(setf optional (queue->list optional))
	(setf keyword (queue->list keyword))
	(setf aux (queue->list aux))

	lambda-list))))

(defun create-lambda-list (list)
  (let ((new-list (make-queue)))
    (flet ((make-required (args)
	     (mapc (rcurry #'enqueue new-list) args))

	   (make-keyword (args)
	     (dolist (arg args)
	       (destructuring-bind (arg key . opts) arg
		 (enqueue (cons (if key (list key arg) arg) opts) new-list)))))

      (with-slots (required optional keyword allow-other-keys rest aux) list
	(make-required required)

	(when optional
	  (enqueue '&optional new-list)
	  (make-required optional))

	(when rest
	  (enqueue '&rest new-list)
	  (enqueue rest new-list))

	(when keyword
	  (enqueue '&key new-list)
	  (make-keyword keyword))

	(when allow-other-keys
	  (enqueue '&allow-other-keys new-list))

	(when aux
	  (enqueue '&aux new-list)
	  (make-required aux))

	(queue->list new-list)))))
