(defpackage #:lazy-susan-test
  (:use #:cl #:lazy-susan)
  (:shadowing-import-from #:lazy-susan
                          #:looks-like-a-number))

(in-package #:lazy-susan-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* (copy-readtable ())))

(defvar *tests* ())

(defun run-tests ()
  (let ((fails 0))
    (declare (special fails))
    (mapcar (lambda (test)
              (format *trace-output* "~&Testing ~A." test)
              (if (funcall test)
                  t))
            (reverse *tests*))
    (format t "~&~A Failures." fails)))

(defmacro test-and (&body forms)
  (let ((f (gensym)))
    (cond ((null forms) t)
          (t `(let ((,f ,(car forms)))
                (declare (special fails))
                (test-and ,@(cdr forms))
                (if  (not ,f)
                     (progn (format t "~&  Failing Form ~A" ',(car forms))
                            (incf fails))
                     (not (plusp fails))))))))

(defmacro deftest (name &body body)
  `(progn (pushnew ',name *tests*)
          (defun ,name ()
            (handler-case (test-and ,@body)
              (error (c) (format t "Failed; threw ~A" c))))))

(defun sllan (s)
  (looks-like-a-number (make-string-input-stream s 1)
                                (schar s 0)))

(deftest it-looks-like-a-number
  (setf (digit-seperators *readtable*) '(#\z))
  (sllan "1z000z000")
  (sllan "1")
  (sllan "1.")
  (sllan "-.1")
  (sllan "11.1")
  (sllan "11.1()")
  (sllan "10e6")
  (sllan "1d0")
  (sllan "1/2  ")
  (sllan "  11/22"))

(deftest does-not-look-numberlike
  (prog1 t (setf (digit-seperators *readtable*) ()))
  (not (sllan "1z000z000"))
  (not (sllan "a1"))
  (not (sllan "  abc"))
  (not (sllan "1/  "))
  (not (sllan "\\1")))

(defun rtfs (s)
  (let ((*package* (find-package :lazy-susan-test)))
    (read-token (make-string-input-stream s))))

(defmacro signals-a (condition &body body)
  `(handler-case (prog1 () ,@body)
     (,condition (c) (declare (ignore c)) t)))

(deftest reading-symbols
  (symbolp (rtfs "lazy-susan:read-token"))
  (symbolp (rtfs "lazy-susan::new?symbol"))
  (eq (rtfs "not-imported") (rtfs "|NOT-IMPORTED|"))
  (eq (rtfs "\\:baz") (rtfs "|:BAZ|"))
  (keywordp (rtfs ":baz"))
  (= (rtfs "1") (rtfs "   1") (rtfs "1   ") 1)
  (signals-a error (rtfs "   "))
  (signals-a reader-error (rtfs "lazy-susan:this-is-not-exported123"))
  (signals-a reader-error (rtfs "too:many:packages"))
  (string-equal (package-name (symbol-package (rtfs "lazy-susan:read-token")))
                "LAZY-SUSAN")
  (string-equal (package-name (symbol-package (rtfs "lazy-susan::internalsymbol")))
                "LAZY-SUSAN"))

(package-local-nickname :lazy-susan-test-cl :cl)

(deftest local-nickname-added
  (symbolp (rtfs "lazy-susan-test:pi"))
  (eq (rtfs "lazy-susan-test:pi") (rtfs "cl:pi"))
  (not (find-package :lazy-susan-test-cl)))

(package-local-nickname :lazy-susan-test-cl2 :cl)
(remove-package-local-nickname :lazy-susan-test-cl2)

(deftest local-nickname-removable
  (signals-a error (rtfs "lazy-susan-test-cl2::package-should-not-be-found")))

(synonym-symbol foo baz)

(synonym-symbol bar cons)

(deftest synonym-symbol-works
  (symbolp (rtfs "foo"))
  (eq (rtfs "foo") (rtfs "baz"))
  (eq (rtfs "bar") (rtfs "cons")))

(synonym-symbol zot zort)
(synonym-symbol flee baz)

(remove-synonym-symbol zot)
(remove-synonym-symbol flee)

(deftest synonym-symbol-removable
  (not (eq (rtfs "zot") (rtfs "zort")))
  (not (eq (rtfs "flee") (rtfs "baz"))))
