;;;; token-reader.lisp

(in-package #:lazy-susan)

;;;; Token Reader
;;; The token-reader is is responsible for creating either a symbol or
;;; a number.  Until we find a way to hijack the dispatch of
;;; constituent characters we have to set the macro-character of
;;; characters that might start a token to lazy-susan:token-reader

;;;; Number Recognition.
;; CLHS 2.3.1 specifies the syntax of numbers
;; We do not yet respect the next section, "potential numbers",
;; wherein implementations are allowed to extend the number syntax.
(defun looks-like-a-number (string)
  "Check to see if the string can be parsed as a number."
  (let ((token (remove-if #'digit-separator-p string)))
    (unless (plusp (length token))
      (return-from looks-like-a-number ()))
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
If it looks like a number we remove the digit-separators and pass that
to be read from a standard readtable. Otherwise we process with lazy-susan's
package and symbol mapping."
  (declare (ignorable count)
           (optimize debug))
  (unless char
    (cerror "NIL instead of a char at token-reader." ())
    (return-from token-reader (values)))
  (multiple-value-bind (package-token
                        name-token
                        saw-escape-p
                        package-markers-seen)
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
            (error 'find-symbol-error
                   :stream stream
                   :symbol name-token
                   :package package-token))))))

(defun collect-token (stream char)
  "Collects the next token as (values package-token name-token saw-escape-p package-markers-seen)
So that token-reader can convert the tokens to a symbol or number. Keywords
result in an empty string for the package token."
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
             do (progn (unless (or (zerop package-markers-seen)
                                   *read-suppress*)
                         (error 'package-marker-error :stream stream))
                       (incf package-markers-seen)
                       (when (package-marker-p (peek-char () stream))
                         (read-char stream)
                         (incf package-markers-seen))
                       (setq char (tokenize-read-char stream))
                       (unless (or char *read-suppress*)
                         (error 'package-marker-error :stream stream))
                       (setq package-token (case-convert token escaped-indices))
                       (go next-token))
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
    (values package-token name-token saw-escape-p package-markers-seen)))

(defun case-convert (string escaped-indices)
  "Case convert the string according to the active readtable-case.
escaped-indices is a list representing which positions in string represent
escaped characters.."
  (let* ((converted (make-array (length string) :element-type 'character
                                :fill-pointer 0))
         (direction  (convert-direction string escaped-indices)))
    (idoveq (i c string converted)
      (vector-push
       (convert i c escaped-indices direction)
       converted))
    converted))

(defgeneric convert (i c escaped-indices direction)
  (:documentation "Convert [c]haracter at [i]ndex given direction, escapes.")
  (:method (i c escaped-indices (direction (eql :upcase)))
    (if (member i escaped-indices) c (char-upcase c)))
  (:method (i c escaped-indices (direction (eql :downcase)))
    (if (member i escaped-indices) c (char-downcase c)))
  (:method (i c escaped-indices (direction (eql :preserve)))
    (declare (ignore i))
    c))

(defun convert-direction (string escaped-indices)
  (if (eq (readtable-case *readtable*) :invert)
      (cond ((idoveq (i c string t)
               (unless (or (member i escaped-indices)
                           (char= c (char-downcase c)))
                 (return nil)))
             ;; designator written downcased, upcase
             :upcase)
            ((idoveq (idx char string t)
               (unless (or (member idx escaped-indices)
                           (char= char (char-upcase char)))
                 (return nil)))
             ;; designator written in capslock, downcase
             :downcase)
            (t ;; designator mixed case, don't change
             :preserve))
      (readtable-case *readtable*)))

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

;;;; Conditions

(define-condition find-symbol-error (package-error reader-error)
  ((symbol :reader find-symbol-error-symbol :initarg :symbol))
  (:report (lambda (c s)
             (let ((errored-s (stream-error-stream c)))
               (format
                s "Error reading stream ~A~@
                   Symbol named ~A is not external to package ~A."
                errored-s
                (find-symbol-error-symbol c)
                (package-error-package c))
               (print-file?-stream-info errored-s :stream s)))))

(define-condition package-marker-error (reader-error)
  ()
  (:report (lambda (c s)
             (format
              s
              "Impermissible pattern of package markers seen while reading ~A."
              (stream-error-stream c))
             (print-file?-stream-info (stream-error-stream c) :stream s))))

(define-condition not-rational-error (reader-error)
  ((symbol :reader not-rational-error-symbol :initarg :symbol)
   (base :reader not-rational-error-base :initarg :base))
  (:report (lambda (c s)
             (let ((errored-s (stream-error-stream c)))
               (format s "Error reading stream ~A~@
                          ~A not a rational in base ~A."
                       errored-s
                       (not-rational-error-symbol c)
                       (not-rational-error-base c))
               (print-file?-stream-info errored-s :stream s)))))

(defun print-file?-stream-info (s &key (stream *standard-output*))
  (when (typep s 'file-stream)
    (format stream "~&In ~S at file position ~A."
            (file-namestring s)
            (file-position s))))
