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
                 (reverse *tests*))))

(defmacro test-and (&body forms)
  (let ((f (gensym)))
    (cond ((null forms) t)
          (t `(let ((,f ,(car forms)))
                (if ,f
                    (test-and ,@(cdr forms))
                    (format t "Failing Form ~A" ',(car forms))))))))

(defmacro deftest (name &body body)
  `(progn (pushnew ',name *tests*)
          (defun ,name () (test-and ,@body))))

(defun sllan (s)
  (looks-like-a-number (make-string-input-stream s 1)
                                (schar s 0)))

(deftest it-looks-like-a-number
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

(defun rtfs (s)
  (read-token (make-string-input-stream s)))

(defmacro signals-a (condition &body body)
  `(handler-case (prog1 () ,@body)
     (,condition (c) (declare (ignore c)) t)))

(deftest reading-symbols
  (symbolp (rtfs "lazy-susan:read-token"))
  (symbolp (rtfs "lazy-susan::new?symbol"))
  (keywordp (rtfs ":baz"))
  (signals-a reader-error (rtfs "lazy-susan:this-is-not-exported123"))
  (signals-a reader-error (rtfs "too:many:packages"))
  (string-equal (package-name (symbol-package (rtfs "lazy-susan:read-token")))
                "LAZY-SUSAN")
  (string-equal (package-name (symbol-package (rtfs "lazy-susan::internalsymbol")))
                "LAZY-SUSAN"))
