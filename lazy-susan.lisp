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
                      (export ',(varsymize n))
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
;;; Issue: we don't as yet recognize invalid syntax.

;;; CLHS 2.1.4 lists syntax types as the following:
;;; constituent, macro character, single escape, invalid,
;;; multiple-escape, whitespace

;; Some syntax types are defined in the same way as constituent traits
(define-constituent-traits
  single-escape (#\\)
  multiple-escape (#\|)
  whitespace (#\Tab #\Newline #\Linefeed #\Page #\Return #\Space))

(defun terminating-macro-character-p (char &optional (readtable *readtable*))
  (multiple-value-bind (fn non-terminating-p) (get-macro-character char readtable)
    (and fn (not non-terminating-p))))

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
         (let ((token (coerce (loop for c = char then
                                    (read-char stream nil nil t)
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
                                 ;; integers can end in a decimal point
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

;; We pretend that there is no difference between read
;; and read preserving whitespace
;; includes much of the reader algorithm from CLHS 2.2
(defun token-reader (stream char &optional count)
  "The reader function used to tokenize a symbol or a number. 
If it is a number we remove the digit-seperators and pass that 
to the underlying lisps tokenizer."
  (declare (ignorable count)
           (optimize debug))
  (when (whitespacep char) (loop for c = char then (read-char stream nil nil t)
                                 while (whitespacep c)
                                 finally (if c (setq char c) (return-from token-reader ()))))
  (when (looks-like-a-number stream char)
    (let ((token (coerce (loop for c = char then (read-char stream nil nil t)
                               while c
                               until (whitespacep c)
                               unless (digit-seperator-p c) collect c
                               finally (unread-char c stream))
                         'string)))
      (let ((*readtable* (load-time-value (copy-readtable nil))))
        (return-from token-reader
          (read-from-string token nil nil)))))
  (let ((package-markers-seen 0)
        package-token
        name-token
        (looks-like-a-keyword (package-marker-p char)))
    (labels ((collect-token ()
               (let ((token (make-array 0 :element-type 'character
                                        :fill-pointer t :adjustable t))
                     (escaped-characters ()))
                 (declare (special escaped-characters))
                 (loop for c = char then (read-char stream nil nil t) while c
                       if (package-marker-p c)
                       do (progn (unless (zerop package-markers-seen)
                                   (error 'reader-error :stream stream))
                                 (incf package-markers-seen)
                                 (when (package-marker-p (peek-char () stream))
                                   (read-char stream)
                                   (incf package-markers-seen))
                                 (setq char (read-char stream))
                                 (setq package-token (case-convert token))
                                 (collect-token)
                                 (return))
                       if (or (terminating-macro-character-p c)
                              (whitespacep c))
                       do (unread-char c stream) (loop-finish)
                       else
                       do (cond ((single-escape-p c)
                                 (push (vector-push-extend
                                        (read-char stream t nil t)
                                        token)
                                       escaped-characters))
                                ((multiple-escape-p c)
                                 (loop for ec = (read-char stream t nil t)
                                       until (multiple-escape-p ec)
                                       do (progn
                                            (when (single-escape-p ec)
                                              (setq ec
                                                    (read-char stream t nil t)))
                                            (push (vector-push-extend ec token)
                                                  escaped-characters))))
                                ((constituentp c) (vector-push-extend c token)))
                       finally (setq name-token (case-convert token)))))
             (case-convert (string)
               (let ((converted (make-array 0 :element-type 'character
                                            :adjustable t :fill-pointer t)))
                 (declare (special escaped-characters))
                 (ecase (readtable-case *readtable*)
                   (:preserve string)
                   (:upcase
                    (idoveq (i c string converted)
                      (vector-push-extend
                       (if (member i escaped-characters) c (char-upcase c))
                       converted)))
                   (:downcase
                    (idoveq (i c string converted)
                      (vector-push-extend
                       (if (member i escaped-characters) c (char-downcase c))
                       converted)))
                   (:invert
                    (cond (;; designator written in capslock, downcase
                           (idoveq (i c string t)
                             (unless (or (member i escaped-characters)
                                         (char= c (char-upcase c)))
                               (return nil)))
                           (idoveq (i c string converted)
                             (vector-push-extend
                              (if (member i escaped-characters)
                                  c
                                  (char-downcase c))
                              converted)))
                          (;; designator written downcased, upcase
                           (idoveq (i c string t)
                             (unless (or (member i escaped-characters)
                                         (char= c (char-downcase c)))
                               (return nil)))
                           (idoveq (i c string converted)
                             (vector-push-extend
                              (if (member i escaped-characters)
                                  c
                                  (char-upcase c))
                              converted)))
                          (t ;; designator mixed case, don't change
                           string)))))))
      (collect-token)
      (if looks-like-a-keyword
          (setq package-token "KEYWORD"))
      (setq package-token (translate-package package-token))
      (multiple-value-bind (symbol status) (find-symbol name-token package-token)
        (if (or symbol status (/= package-markers-seen 1)
                looks-like-a-keyword)
            (canonical-symbol (intern name-token package-token))
            (error 'reader-error :stream stream))))))

(defun read-token (stream)
  "Read a token from stream, return it. Moves stream forward."
  (token-reader stream (read-char stream t)))

(defun translate-package (package-string &optional (package *package*))
  (or (global-package package-string) (package-name package)))

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
    (or (car (assoc local-package-string
                    (gethash package *package-translations*)
                    :test 'string=))
        local-package-string)))

;;;; Synonym Symbols
;;; Also done in a package local fashion.

(defvar *synonym-translations* (make-hash-table))

(defvar *cl-disregards-case* ()
  "Not yet implemented")

(defmacro synonym-symbol (synonym-symbol canonical-symbol
                          &optional (here *package*))
  "Set the symbol to read as another symbol at eval-always time.
  This setting is package-local."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (pushnew (cons ',synonym-symbol ',canonical-symbol)
              (gethash ,here *synonym-translations*)
              :test 'eq :key 'car)))

(defmacro remove-synonym-symbol (synonym-symbol &optional (package *package*))
  "Remove a symbol translation at eval-always time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ,package *synonym-translations*)
           (remove ',synonym-symbol (gethash ,package *synonym-translations*)
                   :key #'car :test #'eq))))

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
