(defpackage #:lazy-susan-test
  (:use #:cl #:lazy-susan)
  (:shadowing-import-from #:lazy-susan
                          #:looks-like-a-number))

(in-package #:lazy-susan-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* (copy-readtable ())))

(defvar *tests* ())

(defparameter *rt* (rt))

(defun run-tests ()
  (let (failing-tests)
    (declare (special failing-tests))
    (mapcar (lambda (test)
              ;(format *trace-output* "~&Testing ~A." test)
              (if (funcall test)
                  t))
            (reverse *tests*))
    (print
     (if failing-tests
         (cons :failed-tests failing-tests)
         :ok))))

(defmacro test-and (&body forms)
  (let ((f (gensym)))
    (cond ((null forms) t)
          (t `(let (,f)
                (declare (special current-test failing-tests))
                (unwind-protect (setq ,f  ,(car forms))
                  (when  (not ,f)
                    (progn (format t "~&  Failing Form ~S" ',(car forms))
                           (push current-test failing-tests))))
                (test-and ,@(cdr forms)))))))

(defmacro deftest (name &body body)
  `(progn (pushnew ',name *tests*)
          (defun ,name ()
            (let ((current-test ',name))
              (declare (special current-test failing-tests))
              (handler-case (test-and ,@body)
                (error (c)
                  (pushnew current-test failing-tests)
                  (format t "Failed; threw ~S~&" c)))))))

(defun sllan (s) (looks-like-a-number s))
(deftest it-looks-like-a-number
  (sllan "1")
  (sllan "1.")
  (sllan "-.1")
  (sllan "11.1")
  (sllan "11.1")
  (sllan "10e6")
  (sllan "1d0")
  (sllan "1/2")
  (sllan "11/22"))

(deftest does-not-look-numberlike
  (not (sllan "1z000z000"))
  (not (sllan "a1"))
  (not (sllan "abc"))
  (not (sllan "1/"))
  (not (sllan "\\1")))

(defun rtfs (s)
  (let ((*package* (find-package :lazy-susan-test))
        (*readtable* *rt*))
    (read (make-string-input-stream s) )))

(defmacro signals-a (condition &body body)
  `(handler-case (prog1 () ,@body)
     (,condition (c) (declare (ignore c)) t)))

(deftest reading-symbols
  (symbolp (rtfs "lazy-susan:token-reader"))
  (symbolp (rtfs "lazy-susan::new?symbol"))
  (eq (rtfs "not-imported") (rtfs "|NOT-IMPORTED|"))
  (eq (rtfs "\\:baz") (rtfs "|:BAZ|"))
  (keywordp (rtfs ":baz"))
  (= (rtfs "1") (rtfs "   1") (rtfs "1   ") 1)
  (signals-a error (rtfs "   "))
  (signals-a reader-error (rtfs "lazy-susan:this-is-not-exported123"))
  (signals-a reader-error (rtfs "too:many:packages"))
  (string-equal (package-name (symbol-package (rtfs "lazy-susan:token-reader")))
                "LAZY-SUSAN")
  (string-equal (package-name (symbol-package (rtfs "lazy-susan::internalsymbol")))
                "LAZY-SUSAN"))

(package-local-nickname :lazy-susan-test-cl :cl)

(deftest local-nickname-added
  (symbolp (rtfs "lazy-susan-test-cl:pi"))
  (eq (rtfs "lazy-susan-test-cl:pi") (rtfs "cl:pi"))
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

;;; Eval always time of synonym-symbols makes following test
;;; clash with previous. 

;; (synonym-symbol zot zort)
;; (synonym-symbol flee baz)

;; (clear-synonym-symbols)

;; (deftest synonym-symbol-removable
;;   (not (eq (rtfs "zot") (rtfs "zort")))
;;   (not (eq (rtfs "flee") (rtfs "baz"))))

(setf (digit-separators *rt*) '(#\*))

(deftest number-readers
  (symbolp (rtfs "*"))
  (zerop (rtfs "#b0"))
  (zerop (rtfs "#o0"))
  (zerop (rtfs "#x0"))
  (= 16
     (rtfs "#b10000")
     (rtfs "#o20")
     (rtfs "#x10"))
  (= 1/2 (rtfs "#b1/10")
     (rtfs "#o10/20")
     (rtfs "#x7/e"))
  (=  (rtfs "#2r1/10")
     (rtfs "#8r10/20")
     (rtfs "#16r7/e"))
  (= (rtfs "#b10000000")
     (rtfs "#b1000*0000")
     (rtfs "#b*1000*0000")
     (rtfs "128")
     (rtfs "1*2*8"))
  (signals-a error (rtfs "#B2"))
  (signals-a error (rtfs "#oa"))
  (signals-a error (rtfs "#x ")))

(deftest read-suppress
  (eq (rtfs "a") (rtfs "#-(and) (progn lt:(bazbux))
                        a"))
  (progn (let ((*read-suppress* t))
           (rtfs "aa:bb:cc:dd")
           (rtfs "aa::"))
         t)
  (signals-a ls::multiple-package-marker-error (rtfs "aa:bb:cc:dd")))

(deftest trailing-keyword-read-form-in-package
  (setf (trailing-package-marker *rt*) :read-form-in-package)
  (equal (rtfs "(car (ls::new-symbol-please))")
         (rtfs "ls:(car (new-symbol-please))"))
  (equal (rtfs "ls:(car (new-symbol-please))")
         (rtfs "ls::(car (new-symbol-please))")))

(deftest trailing-keyword-keyword
  (setf (trailing-package-marker *rt*) :keyword)
  (equal (rtfs ":foo") (rtfs "foo:"))
  (equal (rtfs "foo: ") (rtfs "foo:("))
  (signals-a error (rtfs "foo:: ")))

(deftest trailing-keyword-nil
  (prog1 t (setf (trailing-package-marker *rt*) ()))
  (signals-a ls::trailing-package-marker-error (rtfs "foo: (")))
