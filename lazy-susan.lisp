;;;; lazy-susan.lisp

(in-package #:lazy-susan)

(syntax-bind ()
  (#\# #\;) 'comment-line-suppress-forms)

;;;; Constituent Traits
;;; See CLHS 2.1.4.1, 2.1.4.2
;;; The ability to customize constituent traits is a reason for this package.
;;; If a constituent trait has a special variable it can be customized
;;; by shadowing that list of characters.

;;; List of constituent traits from CLHS:
;;; alphabetic, digit, package marker, plus sign,
;;; minus sign, dot, decimal point, ratio marker, exponent marker, invalid. 

;;; Departure from the standard: We don't include the invalid traits, and
;;; alphabetic is a fall through for anything that wasn't one of the others.
;;; We also introduce a *digit-seperator*, which can be included arbitrarily
;;; in a representation of number without changing the value parsed by the reader.

(defmacro define-constituent-traits (&body name/chars)
  (flet ((valsymize (s)
           (setq s (symbol-name s))
           (intern (concatenate 'string s (if (find #\- s)
                                              (symbol-name '#:-p)
                                              (symbol-name '#:p)))))
         (varsymize (s)
           (intern (concatenate 'string "*" (symbol-name s) "*"))))
    `(progn ,@(loop for (n cs) on name/chars by #'cddr
                    appending
                    `((defvar ,(varsymize n) (list ,@cs))
                      (defun ,(valsymize n) (c)
                        (member c ,(varsymize n) :test #'char-equal)))))))

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
  ;; e.g. 1,000,000 = 1000000
  digit-seperator (#\,)
  )

;;;; Syntax Types
;;; Issue: I don't see an easy way to distinquish between a terminating
;;; and a nonterminating macro character.

;;; CLHS 2.1.4 lists syntax types as the following:
;;; constituent, macro character, single escape, invalid,
;;; multiple-escape, whitespace

;; Some syntax types are defined in the same way as constituent traits
(define-constituent-traits
  single-escape (#\\)
  multiple-escape (#\|)
  whitespace (#\Tab #\Newline #\Linefeed #\Page #\Return #\Space))

;; todo fix this
(defun constituentp (c)
  c)

;;;; Token Reader
;;; The token reader is entered when a character of type constituent or escape
;;; is dispatched on by the readtable. It is responsible for creating either a
;;; symbol or a number.

;;; Creating our own version of this allows us to customize the way in which
;;; symbols and numbers are read, such as allowing us to create package local
;;; package nicknames, synonym symbols, and a visual marker for grouping digits
;;; in large numbers

;; CLHS 2.3.1 specifies the syntax of numbers
;; We do not yet respect the next section, "potential numbers",
;; wherein implementations are allowed to extend the number syntax.
(defun looks-like-a-number (stream char)
  "Check to see if the next token should be parsed as a number"
  (let ((start (file-position stream)))
    (unwind-protect
         (let ((token (coerce (loop for c = char then (read-char stream nil nil t)
                                    while c
                                    until (whitespacep c) collect c)
                              'string)))
           (setq token (remove-if #'digit-seperator-p token))
           (when (or (plus-sign-p (schar token 0))
                     (minus-sign-p (schar token 0)))
             ;; it's valid syntax for any number to start with single sign
             (setq token (subseq token 1)))
           (labels ((decimal-digit-p (c) (find c "0123456789"))
                    (digitp (c) (digit-char-p c *read-base*))
                    (possible-integer (s)
                      (let ((l (length s)))
                        (and (plusp l)
                             (every #'digitp (subseq s 0 (1- l)))
                             (or (digitp (char s (1- l)))
                                 (decimal-point-p (char s (1- l)))))))
                    (possible-ratio (s)
                      (let ((ratio-position (position-if #'ratio-marker-p s)))
                        (when ratio-position
                          (let ((numerator (subseq s 0 ratio-position))
                                (denominator (subseq s (1+ ratio-position))))
                            (and
                             (every #'digitp numerator)
                             (every #'digitp denominator)
                             (plusp (length numerator))
                             (plusp (length denominator)))))))
                    (possible-exponent (s)
                      ;; clhs 2.3.1 specifies the exponent digits to be a digit,
                      ;; but sbcl and clisp both treat it as a /decimal/ digit.
                      ;; we follow sbcl and clisp in breaking from the standard
                      ;; because the standard appears to be an editorial error
                      ;; rather than desired behavior.
                      (when (exponent-marker-p (schar s 0))
                        (setq s (subseq s 1)) ;; drop exponent marker
                        (let ((s-no-sign (string-left-trim
                                          (append *minus-sign* *plus-sign*)
                                          s)))
                          ;; maximum one sign
                          (and (<= (- (length s) (length s-no-sign)) 1)
                               (plusp (length s-no-sign))
                               (every #'decimal-digit-p s-no-sign)))))
                    (possible-float (s)
                      (let ((decimal-point-position
                             (position-if #'decimal-point-p s))
                            (exponent-marker-position
                             (position-if #'exponent-marker-p s)))
                        (and (if decimal-point-position
                                 (let ((whole
                                        (subseq s 0 decimal-point-position))
                                       (decimal
                                        (subseq s (1+ decimal-point-position)
                                                exponent-marker-position)))
                                   (and (every #'decimal-digit-p whole)
                                        (every #'decimal-digit-p decimal)
                                        (or (plusp (length whole))
                                            (plusp (length decimal)))))
                                 ;; else
                                 (and exponent-marker-position
                                      (every #'decimal-digit-p
                                             (subseq
                                              s 0 exponent-marker-position))))
                             (if exponent-marker-position
                                 (possible-exponent
                                  (subseq s exponent-marker-position))
                                 t)))))
             ;; return of the function
             (or (possible-integer token)
                 (possible-ratio token)
                 (possible-float token))))
      ;; unwind-protected form
      (file-position stream start))))

;; Higher level read call should deal with whitespace preservation semantics,
;; not the token reader. 
(defun token-reader (stream char &optional count)
  "The reader function used to tokenize a symbol or a number. 
If it is a number we remove the digit-seperators and pass that 
to the underlying lisps tokenizer."
  (declare (ignorable count))
  (when (looks-like-a-number stream char)
    )
  (let* ((token (make-array 1 :initial-element char :element-type 'character
                            :fill-pointer t :adjustable t))
         (escaped-characters ()))
    (loop for c = (read-char stream nil nil t)
                      ;with in-single-escape = nil
                      ;with in-multiple-escape = nil
                      while c
                      do (cond ((single-escape-p c)
                                (push (fill-pointer token) escaped-characters)
                                (vector-push-extend (read-char stream t nil t)
                                                    token))
                               ((multiple-escape-p c)
                                (loop for ec = (read-char stream t nil t)
                                      until (multiple-escape-p c)
                                      do (progn (when (single-escape-p ec)
                                                  (setq ec (read-char stream t nil t)))
                                                (push (fill-pointer token)
                                                      escaped-characters)
                                                (vector-push-extend ec token))))
                               ((constituentp c) (vector-push-extend c token))))
    token))


#;
(defun local-package-name (name &key (package *package*) (readtable *readtable))
  ())

#2; Trying #'constituent-reader instead
(defun reader-algorithm (stream &optional (readtable *readtable*))
  (let ((c #1=(read-char stream)))
    (flet ((advance () (setq c #1#)))
      (prog () 
       :1 (when (eofp c)
	    (eof-processing))
       :2 (when (invalid-character-p x)
	    (error 'reader-error))
       :3 (when (whitespace-char-p c)
	    (advance)
	    (go :1))
       :4 (whereas* ((function (get-macro-character c readtable))
		     (val-list (multiple-value-list (funcall function stream c))))
	    (if val-list
		(return-from reader-algorithm (car val-list))
		(go :1)))
       :5 (when (single-escape-p c)
	    (begin-token (advance))
	    (go :8))
       :6 (when (multiple-escape-p c)
	    (go :9))
       :7 (when (constituentp c)
	    (go :8))
       :8 ))))

(defclass-autoargs lazy-susan-readtable ()
  (lazy-susan-case))

