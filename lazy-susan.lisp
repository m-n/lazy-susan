;;;; lazy-susan.lisp

;;; This file provides the implementation of package-local-nicknames,
;;; synonym-symbols, and a basic lazy-susan readtable generator
;;; rt abbreviates readtable.

(in-package #:lazy-susan)

;;;; Package Local Nicknames
;;; Inspired by or borrowed from sbcl's api
(defvar *package-translations* (make-hash-table))

(defmacro package-local-nickname
    (local-nickname actual-package &optional (package *package*))
  "Add a package local nickname at eval-always time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew (cons ',local-nickname ',actual-package)
              (gethash ,package *package-translations*)
              :test 'equal)))

(defmacro remove-package-local-nickname
    (local-nickname &optional (package *package*))
  "Remove a package local nickname at eval-always time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,package *package-translations*)
           (remove ',local-nickname (gethash ,package *package-translations*)
                   :key #'car :test #'string=))))

(defun package-local-nicknames (&optional (here *package*))
  "Return the package local nicknames of this package as ((nn . long-name) ...) "
  (gethash here *package-translations*))

(defun package-local-names (package &optional (here *package*))
  "Return a list of the local, local names of package."
  (when (packagep package) (setq package (package-name package)))
  (mapcar #'car (remove-if-not (lambda (p) (string= p package))
                               (gethash here *package-translations*)
                               :key 'cdr)))

(defun global-package (local-package-string &optional (package *package*))
  ;; when guards against confusing nil and "NIL"
  (when local-package-string
    (or (cdr (assoc local-package-string
                    (gethash package *package-translations*)
                    :test 'string=))
        local-package-string)))

;;;; Synonym Symbols

(defvar *synonym-translations* (make-hash-table))

(defmacro synonym-symbol (synonym-symbol canonical-symbol
                          &optional (here *package*))
  "Set the symbol to read as another symbol at eval-always time.
  This setting is package-local."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew (cons ',synonym-symbol ',canonical-symbol)
              (gethash ,here *synonym-translations*)
              :test 'eq :key 'car)))

(defmacro clear-synonym-symbols (&optional (package *package*))
  "Remove symbol translations from package eval-always time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,package *synonym-translations*) ())))

(defun local-synonym-alist (&optional (here *package*))
  "Return the synonyms defined in this package as ((synonym . real-symbol) ...) "
  (gethash here *synonym-translations*))

(defun local-synonyms (canonical-symbol &optional (here *package*))
  "Return a list of the synonym symbols of a canonical symbol."
  (mapcar #'car (remove-if-not (lambda (n) (eq n canonical-symbol))
                               (gethash here *synonym-translations*)
                               :key 'cdr)))

(defun canonical-symbol (synonym &optional (package *package*))
  (or (cdr (assoc synonym
                  (gethash package *synonym-translations*)
                  :test 'eq))
      synonym))

;;;; A Readtable

;;; There are two aspects to this: first, we have to set constituent
;;; characters to our macro character so we can shadow the symbol
;;; reading semantics. Second, we have to make our own string and
;;; number macro character readers and use them because their
;;; semantics depend on constituent traits.

(defun rt (&optional (rt (load-time-value (copy-readtable nil))))
  "Return copy of ReadTable with lazy-susan features enabled. ASCII only.
  This sets non-whitespace, non-macro characters with code point 0 to 200
  to be the lazy-susan's token-reader. Better solutions solicited."
  (prog1 (setq rt (copy-readtable rt))
    (loop for i from 0 to 126
          for char = (code-char i)
          unless (or (get-macro-character char rt)
                     (whitespacep char)) do
          (set-macro-character char #'token-reader t rt))
    ;; Strings are specified to use single-escape characters as escape
    ;; So we have to shadow #\" to make it work with our idea of a single escape
    (set-macro-character #\" #'read-string () rt)
    (set-dispatch-macro-character #\# #\b #'read-rational rt)
    (set-dispatch-macro-character #\# #\o #'read-rational rt)
    (set-dispatch-macro-character #\# #\x #'read-rational rt)
    (set-dispatch-macro-character #\# #\r #'read-rational rt)))

(defun read-string (stream char &optional count)
  (declare (ignore count))
  (let ((end-char (closer char)))
    (with-output-to-string (s)
      (loop for c = (read-char stream t t t)
            until (char= c end-char) do
            (write-char (if (single-escape-p c)
                            (read-char stream t t t)
                            c)
                        s)))))

(defun read-rational (stream char &optional count)
  "Read a rational, set the *read-base* according to the standard syntax's
  idea of what it should be based on the macro-character."
  (let ((*read-base* (ecase char
                       ((#\b #\B) 2)
                       ((#\o #\O) 8)
                       ((#\x #\X) 16)
                       ((#\r #\R) count))))
    (multiple-value-bind (package token escapep package-marker-count)
        (collect-token stream (read-char stream t t t))
      (unless *read-suppress*
        (unless (and (not (or package escapep))
                     (zerop package-marker-count))
          (cerror "Try to make a number anyway."
                  "Package or escape character seen while parsing rational."))
        (let ((number (looks-like-a-number token)))
          (assert (rationalp number) (number)
                  "~A not a rational." number)
          number)))))

;;;; Convenient way to use a rt

(defmacro in-project (package-designator &optional rt)
  "IN-PACKAGE alternative. Also sets *readtable*.
  Default RT set by (setf project-rt)."
  (with-gensyms (r p)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let  ((,r ,rt)
              (,p ',package-designator))
         (unless ,r (setq ,r (project-rt ,p)))
         (prog1 (setq *package* (find-package ,p))
           (setq *readtable* ,r))))))

(defvar *project-rts* (make-hash-table :test 'equal))

(defun project-rt (package-designator)
  (gethash (find-package package-designator) *project-rts* *readtable*))

(defsetf project-rt (package-designator) (rt)
  "Set a packages default readtable for use with in-project."
  `(setf (gethash (find-package ,package-designator) *project-rts*)
         ,rt))
