(defpackage #:lazy-susan-test
  (:use #:cl #:yarty)
  (:import-from #:lazy-susan
                #:looks-like-a-number))

(in-package #:lazy-susan-test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* (copy-readtable ()))
  (defparameter *rt* (ls:rt)))

(defun sllan (s)
  (let ((*readtable* *rt*))
    (numberp (looks-like-a-number s))))

(deftest/each it-looks-like-a-number
  (sllan "1")
  (sllan "1.")
  (sllan "-.1")
  (sllan "11.1")
  (sllan "11.1")
  (sllan "10e6")
  (sllan "1d0")
  (sllan "1/2")
  (sllan "11/22")
  (progn (setf (ls:decimal-points *rt*) '(#\!))
         (sllan "1!1"))
  (not (sllan "1.1"))
  (progn (setf (ls:decimal-points *rt*) '(#\.))
         (sllan "1.1")))

(deftest/each does-not-look-numberlike
  (not (sllan "1z000z000"))
  (not (sllan "a1"))
  (not (sllan "abc"))
  (not (sllan "1/"))
  (not (sllan "\\1")))

(defun rtfs (s)
  (let ((*package* (find-package :lazy-susan-test))
        (*readtable* *rt*))
    (read (make-string-input-stream s) )))

(deftest/each reading-symbols
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
  (string= (package-name (symbol-package (rtfs "lazy-susan::internalsymbol")))
           "LAZY-SUSAN")
  (string= (rtfs "#:foo") "FOO")
  (string= (rtfs "#:f\\oo") "FoO")
  (null (symbol-package (rtfs "#:foo"))))

(ls:add-package-local-nickname :lazy-susan-test-cl :cl)

(deftest/each local-nickname-added
  (symbolp (rtfs "lazy-susan-test-cl:pi"))
  (eq (rtfs "lazy-susan-test-cl:pi") (rtfs "cl:pi"))
  (not (find-package :lazy-susan-test-cl)))

(ls:add-package-local-nickname :lazy-susan-test-cl2 :cl)
(ls:remove-package-local-nickname :lazy-susan-test-cl2)

(deftest/each local-nickname-removable
  (signals-a error (rtfs "lazy-susan-test-cl2::package-should-not-be-found")))

(ls:add-symbol-package-marker 'tester :cl)

(deftest/each symbol-package-marker-added
  (unwind-protect
       (progn (setf (ls:package-resolution-strategy *rt*) 'ls:spm)
              (eq (symbol-package (rtfs "tester:defun"))
                  (find-package "CL")))
    (setf (ls:package-resolution-strategy *rt*) 'ls:package-local)))

(ls:add-symbol-package-marker 'tester2 :cl)
(ls:remove-symbol-package-marker 'tester2)

(deftest/each symbol-package-marker-removable
  (unwind-protect
       (progn (setf (ls:package-resolution-strategy *rt*) 'ls:spm)
              (signals-a package-error (rtfs "tester2:defun")))
    (setf (ls:package-resolution-strategy *rt*) 'ls:package-local)))

(ls:add-rt-package-translation "RTL" :cl *rt*)

(deftest/each rt-package-translation-addable
  (unwind-protect
       (progn (setf (ls:package-resolution-strategy *rt*) 'ls:rt-local)
              (eq (symbol-package (rtfs "rtl:defun"))
                  (find-package "CL")))
    (setf (ls:package-resolution-strategy *rt*) 'ls:package-local)))

(ls:add-rt-package-translation "RTL-REM" :cl *rt*)
(ls:remove-rt-package-translation "RTL-REM" *rt*)

(deftest/each rt-package-translation-removable
  (unwind-protect
       (progn (setf (ls:package-resolution-strategy *rt*) 'ls:rt-local)
              (signals-a package-error (rtfs "rtl-rem:defun")))
    (setf (ls:package-resolution-strategy *rt*) 'ls:package-local)))

(ls:add-synonym-symbol foo baz)

(ls:add-synonym-symbol bar cons)

(deftest/each synonym-symbol-works
  (symbolp (rtfs "foo"))
  (eq (rtfs "foo") (rtfs "baz"))
  (eq (rtfs "bar") (rtfs "cons")))

;;; Eval always time of synonym-symbols makes following test
;;; clash with previous. 

;; (synonym-symbol zot zort)
;; (synonym-symbol flee baz)

;; (clear-synonym-symbols)

;; (deftest/each synonym-symbol-removable
;;   (not (eq (rtfs "zot") (rtfs "zort")))
;;   (not (eq (rtfs "flee") (rtfs "baz"))))

(setf (ls:digit-separators *rt*) '(#\*))

(deftest/each number-readers
  (symbolp (rtfs "*"))
  (eql (- 1) (rtfs "-1"))
  (symbolp (rtfs "--1"))
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

(deftest/each read-suppress
  (eq (rtfs "a") (rtfs "#-(and) (progn lt:(bazbux))
                        a"))
  (progn (let ((*read-suppress* t))
           (rtfs "aa:bb:cc:dd")
           (rtfs "aa::"))
         t)
  (signals-a ls::multiple-package-marker-error (rtfs "aa:bb:cc:dd")))

(deftest/each trailing-keyword-read-form-in-package
  (setf (ls:trailing-package-marker *rt*) :read-form-in-package)
  (equal (rtfs "(car (ls::new-symbol-please))")
         (rtfs "ls:(car (new-symbol-please))"))
  (equal (rtfs "ls:(car (new-symbol-please))")
         (rtfs "ls::(car (new-symbol-please))"))
  (null (rtfs "#+(or)irrelevent::t ()")))

(deftest/each trailing-keyword-keyword
  (setf (ls:trailing-package-marker *rt*) :keyword)
  (equal (rtfs ":foo") (rtfs "foo:"))
  (equal (rtfs "foo: ") (rtfs "foo:("))
  (signals-a error (rtfs "foo:: ")))

(deftest/each trailing-keyword-nil
  (prog1 t (setf (ls:trailing-package-marker *rt*) ()))
  (signals-a ls::trailing-package-marker-error (rtfs "foo: (")))
