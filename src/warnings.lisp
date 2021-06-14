;;;; warnings.lisp
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

;;;; Declarations controlling when warnings are emitted

(in-package #:static-dispatch)

(define-declaration static-dispatch-warn (args)
  "Controls the level of warnings emitted when static dispatching.

   Takes one argument the level which can be one of the following
   symbols:

     ALL - Emit a warning if static dispatching fails for whatever
           reason.

     NONE - Do not emit any warnings"

  (destructuring-bind (level) args
    (check-type level symbol)

    (let ((level (intern (symbol-name level) (find-package :static-dispatch))))
      (unless (typep level '(member all none))
        (setf level nil)
        (simple-style-warning "Unrecognized static-dispatch warning level: ~a" level))

      (values
       :declare
       (cons 'static-dispatch-warn level)))))

(defun should-warn? (env)
  "Returns true if warnings should be emitted for static-dispatching
   errors in the environment ENV."

  (let ((level (declaration-information 'static-dispatch-warn env)))
    (not (eq level 'none))))

(defun dispatch-warn (env name e)
  "Emit a static dispatch failure warning.

   Checks whether the warning should actually be emitted, according to
   the declared static-dispatch warning level in the environment.

   ENV is the lexical environment of the generic function call.

   E is the error condition object, of the static-dispatch failure,
   which is printed in the warning."

  (when (should-warn? env)
    (simple-style-warning "Static dispatch for ~s failed:~%~2T~a" name e)))
