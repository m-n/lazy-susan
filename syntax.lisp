;;;; syntax.lisp

(in-package #:lazy-susan)

;;;; Constituent Traits
;;; See CLHS 2.1.4.1, 2.1.4.2
;;; The ability to customize constituent traits is a motivation for lazy-susan.

(defmacro define-constituent-traits (&body name/chars)
  " Return something like :
    (progn 
      (defvar *package-marker* (make-hash-table :test #'eq))

      (defun package-marker-p (c &optional (rt *readtable*))

      (find c (package-markers rt) :test #'char-equal))

      (defun package-markers (rt) (gethash rt *package-marker* (list #\:)))

      (defsetf package-markers (rt) (list-of-chars)
       \"Within readtable, represent trait by characters.\"
        `(setf (gethash ,rt *package-marker*) ,list-of-chars))

      ...)
  Where only the setf method is exposed functionality."
  (flet ((valsymize (s)
           (setq s (symbol-name s))
           (intern (concatenate 'string s (if (find #\- s)
                                              (symbol-name '#:-p)
                                              (symbol-name '#:p)))))
         (varsymize (s)
           (intern (concatenate 'string "*" (symbol-name s) "*")))
         (pluralize (s)
           (intern (concatenate 'string (symbol-name s) (symbol-name '#:s)))))
    `(progn ,@(loop for (n cs) on name/chars by #'cddr
                    appending
                    `((defvar ,(varsymize n) (make-hash-table :test #'eq))
                      (defun ,(valsymize n) (c &optional (rt *readtable*))
                        (find c (,(pluralize n) rt) :test #'char-equal))
                      (defun ,(pluralize n) (rt)
                        (gethash rt ,(varsymize n) ',cs))
                      (defsetf ,(pluralize n) (rt) (list-of-chars)
                        "Within readtable, represent trait by characters."
                        `(setf (gethash ,rt ,',(varsymize n))
                               ,list-of-chars)))))))

;;; List of constituent traits from CLHS:
;;; alphabetic, digit, package marker, plus sign,
;;; minus sign, dot, decimal point, ratio marker, exponent marker, invalid. 

;;; Departure from the standard: We don't include the invalid traits, and
;;; alphabetic is a fall through for anything that wasn't one of the others.
;;; We also introduce the digit-separator, which can be included arbitrarily
;;; in a representation of number without changing the value lexed by the reader.

(define-constituent-traits
  package-marker (#\:)
  plus-sign (#\+)
  minus-sign (#\-)
  ;; the consing dot is not relevant to us
  ;dot (#\.)
  decimal-point (#\.)
  ratio-marker (#\/)
  exponent-marker (#\d #\e #\f #\l #\s)
  ;"invalid" would include whitespace, rubout, backspace
  ;;A character that can be included in a number without chaning its semantics, 
  ;; e.g. 1,000,000 reads as 1000000 if we used #\,
  digit-separator ()
  )

;;;; Syntax Types
;;; Issue: we don't yet recognize invalid syntax.

;;; CLHS 2.1.4 lists syntax types as the following:
;;; constituent, macro character, single escape, invalid,
;;; multiple-escape, whitespace

;;; Per the clhs these are supposed to be mutually exclusive,
;;; but I want to be able to use a macro character as a single escape
;;; (especially inside strings), so we don't define them like
;;; constituent traits instead
(define-constituent-traits
  single-escape (#\\)
  multiple-escape (#\|)
  whitespace (#\Tab #\Newline #\Linefeed #\Page #\Return #\Space))

(defun terminating-macro-character-p (char &optional (rt *readtable*))
  (multiple-value-bind (fn non-terminating-p) (get-macro-character char rt)
    (and fn (not non-terminating-p))))

(defun constituentp (c)
  "Wrong, but works internally. "
  c)
