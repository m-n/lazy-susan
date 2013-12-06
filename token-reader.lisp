;;;; token-reader.lisp

(in-package #:lazy-susan)

;;;; Token Reader
;;; The token-reader is is responsible for creating either a
;;; symbol or a number.

;;; Until we find a way to hijack the dispatch of constituent characters we
;;; have to set the macro-character of characters that might start a token
;;; to lazy-susan:token-reader

;;; Creating our own token reader allows us to customize the way in which
;;; symbols and numbers are read, such as allowing us to create package local
;;; package nicknames, synonym symbols, and a visual marker for grouping digits
;;; in large numbers

(defun chomp-whitespace (stream char)
  (cond ((whitespacep char)
         (loop for c = (read-char-no-hang stream () () t)
               until (or (not c) (not (whitespacep c)))
               finally (return c)))
        (t char)))

;;;; Number Recognition.
;; CLHS 2.3.1 specifies the syntax of numbers
;; We do not yet respect the next section, "potential numbers",
;; wherein implementations are allowed to extend the number syntax.
(defun looks-like-a-number (string)
  "Check to see if the string can be parsed as a number."
  (let ((token (remove-if #'digit-seperator-p string)))
    (when (or (plus-sign-p (schar token 0))
              (minus-sign-p (schar token 0)))
      ;; it's valid syntax for any number to start with single sign
      (setq token (subseq token 1)))
    (and (or (possible-integer token)
             (possible-ratio token)
             (possible-float token))
         token
         (let ((*readtable* (load-time-value (copy-readtable nil))))
           (read-from-string token nil nil)))))

(defun decimal-digit-p (c) (find c "0123456789"))

(defun digitp (c) (digit-char-p c *read-base*))

(defun possible-integer (s)
  (let ((l (length s)))
    (and (plusp l)
         (every #'digitp (subseq s 0 (1- l)))
         (or (digitp (char s (1- l)))
             ;; integers can end in a decimal point
             (decimal-point-p (char s (1- l)))))))

(defun possible-ratio (s)
  (let ((ratio-position (position-if #'ratio-marker-p s)))
    (when ratio-position
      (let ((numerator (subseq s 0 ratio-position))
            (denominator (subseq s (1+ ratio-position))))
        (and (every #'digitp numerator)
             (every #'digitp denominator)
             (plusp (length numerator))
             (plusp (length denominator)))))))

(defun possible-exponent (s)
  ;; clhs 2.3.1 specifies the exponent digits to be a digit,
  ;; but sbcl and clisp both treat it as a /decimal/ digit.
  ;; we follow sbcl and clisp in breaking from the standard
  ;; because the standard appears to be an editorial error
  ;; rather than desired behavior, and because the underlying
  ;; lisp will be called upon to make the number anyway
  (when (exponent-marker-p (schar s 0))
    (setq s (subseq s 1)) ;; drop exponent marker
    (let ((s-no-sign (string-left-trim
                      (append (minus-signs *readtable*)
                              (plus-signs *readtable*))
                      s)))
      ;; maximum one sign
      (and (<= (- (length s) (length s-no-sign)) 1)
           (plusp (length s-no-sign))
           (every #'decimal-digit-p s-no-sign)))))

(defun possible-float (s)
  (let ((decimal-point-position (position-if #'decimal-point-p s))
        (exponent-marker-position (position-if #'exponent-marker-p s)))
    (and (if (and decimal-point-position
                  exponent-marker-position)
             (< decimal-point-position
                exponent-marker-position)
             t)
         (if decimal-point-position
             (let ((whole (subseq s 0 decimal-point-position))
                   (decimal (subseq s (1+ decimal-point-position)
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
             (possible-exponent (subseq s exponent-marker-position))
             t))))

;;;; Tokenization from a stream

(defun token-reader (stream char &optional count)
  "The reader function used to tokenize a symbol or a number. 
If it looks like a number we remove the digit-seperators and pass that 
to be read from a standard readtable. Otherwise we process with lazy-susan's
package and symbol mapping."
  (declare (ignorable count)
           (optimize debug))
  (unless char
    (cerror "NIL instead of a char at token-reader." ())
    (return-from token-reader (values)))
  (multiple-value-bind (package-token name-token saw-escape-p package-markers-seen)
      (collect-token stream char)
    (unless *read-suppress*
      (unless (or package-token saw-escape-p)
        ;; if it's a number return it
        (let ((number (looks-like-a-number name-token)))
          (when number
            (return-from token-reader number))))
      (setq package-token (translate-package package-token))      
      (multiple-value-bind (symbol status)
          (find-symbol name-token package-token)
        (if (or symbol status (/= package-markers-seen 1)
                (equal package-token "KEYWORD"))
            (canonical-symbol (intern name-token package-token))
            (error 'reader-error :stream stream))))))

(defun collect-token (stream char)
  "Collects the next token as 
   (values package-token name-token saw-escape-p package-markers-seen)
So that token-reader can convert the tokens to a symbol or number."
  (declare (optimize debug))
  (let ((package-markers-seen 0)
        package-token
        name-token
        saw-escape-p)
    (tagbody next-token ;; so we can use same loop for package, then symbol
       (loop for c = char then (tokenize-read-char stream) while c
             with token = (make-array 0 :element-type 'character
                                      :fill-pointer t :adjustable t)
             with escaped-indices = ()        
             if (package-marker-p c)
             do (progn (unless (zerop package-markers-seen)
                         (error 'reader-error :stream stream))
                       (incf package-markers-seen)
                       (when (package-marker-p (peek-char () stream))
                         (read-char stream)
                         (incf package-markers-seen))
                       (setq char (tokenize-read-char stream))
                       (unless char 
                         (error 'reader-error :stream stream))
                       (setq package-token (case-convert token escaped-indices))
                       (go next-token))
             if (or (terminating-macro-character-p c)
                    (whitespacep c))
             do (loop-finish)
             else
             do (cond ((single-escape-p c)
                       (setq saw-escape-p t)
                       (push (vector-push-extend
                              (read-char stream t nil t)
                              token)
                             escaped-indices))
                      ((multiple-escape-p c)
                       (setq saw-escape-p t)
                       (loop for ec = (read-char stream t nil t)
                             until (multiple-escape-p ec)
                             do (progn
                                  (when (single-escape-p ec)
                                    (setq ec
                                          (read-char stream t nil t)))
                                  (push (vector-push-extend ec token)
                                        escaped-indices))))
                      ((constituentp c) (vector-push-extend c token)))
             finally (setq name-token (case-convert token escaped-indices))))
    (when (and package-token (not name-token))
      ;; assumed name was a package, no package given
      (rotatef package-token name-token))
    (values package-token name-token saw-escape-p package-markers-seen)))

(defun case-convert (string escaped-indices)
  "Case convert the string according to the active readtable-case.
escaped-indices is a list representing which positions in string represent
escaped characters.."
  (let ((converted (make-array (length string) :element-type 'character
                               :fill-pointer 0)))
    (ecase (readtable-case *readtable*)
      (:upcase (idoveq (i c string converted)
                 (vector-push
                  (if (member i escaped-indices) c (char-upcase c))
                  converted)))
      (:downcase (idoveq (i c string converted)
                   (vector-push
                    (if (member i escaped-indices) c (char-downcase c))
                    converted)))
      (:invert (cond ((idoveq (i c string t)
                        (unless (or (member i escaped-indices)
                                    (char= c (char-upcase c)))
                          (return nil)))
                      ;; designator written in capslock, downcase
                      (idoveq (i c string converted)
                        (vector-push
                         (if (member i escaped-indices)
                             c
                             (char-downcase c))
                         converted)))
                     ((idoveq (i c string t)
                        (unless (or (member i escaped-indices)
                                    (char= c (char-downcase c)))
                          (return nil)))
                      ;; designator written downcased, upcase
                      (idoveq (i c string converted)
                        (vector-push
                         (if (member i escaped-indices)
                             c
                             (char-upcase c))
                         converted)))
                     (t ;; designator mixed case, don't change
                      (idoveq (i c string converted)
                        (vector-push
                         c
                         converted))
                      converted)))
      (:preserve (idoveq (i c string converted)
                   (vector-push
                    c
                    converted))
                 converted))))

(defun tokenize-read-char (stream)
  "Read the character if it will continue the token, otherwise return nil."
  (let ((c (peek-char nil stream nil nil t)))
    (unless (or (not c)
                (terminating-macro-character-p c)
                (whitespacep c))
      (read-char stream t nil t))))

(defun translate-package (package-string &optional (package *package*))
  "Translate the case-converted package string into the argument to be passed to intern."
  (or (and package-string
           (= (length package-string) 0)
           "KEYWORD")
      (global-package package-string)
      (package-name package)))

