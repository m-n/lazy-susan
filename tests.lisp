(defpackage #:lazy-susan-test
  (:use #:cl #:lazy-susan)
  (:shadowing-import-from #:lazy-susan
                          #:looks-like-a-number))

(in-package #:lazy-susan-test)

(defvar *tests* ())

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (test)
                   (format *trace-output* "~&Testing ~A." test)
                   (if (funcall test)
                       (progn (format *trace-output* " Passed.") t)
                       (format *trace-output* " FAILED")))
                 *tests*)))

(defmacro deftest (name &body body)
  `(progn (pushnew ',name *tests*)
          (defun ,name () (and ,@body))))

(defun sllan (s)
  (looks-like-a-number (make-string-input-stream s 1)
                                (schar s 0)))

(deftest looks-like-a-number-1
  (sllan "1")
  (sllan "1.")
  (sllan "11.1")
  (sllan "1,000,000")
  (sllan "10e6")
  (sllan "1d0")
  (sllan "1/2")
  (sllan "11/22"))

(deftest does-not-look-numberlike
  (not (sllan "a1"))
  (not (sllan "abc"))
  (not (sllan "1/"))
  (not (sllan "\\1")))
