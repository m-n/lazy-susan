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
  "Check to see if the string can be parsed as a number. If so, return the number."
  (let* ((token (remove-if #'digit-separator-p string))
         (signless (progn (unless (plusp (length token))
                            (return-from looks-like-a-number ()))
                          (if (or (plus-sign-p (schar token 0))
                                  (minus-sign-p (schar token 0)))
                              (subseq token 1)
                              token))))
    (and (or (possible-integer signless)
             (possible-ratio signless)
             (possible-float signless))
         signless
         (let* (;; standardized-token must be done within dynamic
                ;; scope of the *readtable* for which we are parsing
                ;; the number
                (standardized-token (standard-number-syntax token))
                (*readtable* (load-time-value (copy-readtable nil))))
           (read-from-string standardized-token nil nil)))))

(defun standard-number-syntax (string)
  "Take a string, whose digit separators have already been removed,
and replace number related syntax of the current rt with standard syntax."
  (let ((out (make-array (length string) :element-type 'character
                         :fill-pointer 0 )))
    (loop for c across string
          do (vector-push (cond ((decimal-point-p c) #\.)
                                ((plus-sign-p c) #\+)
                                ((minus-sign-p c) #\-)
                                ((ratio-marker-p c) #\/)
                                ;; oops, what's it mean to replace one of these?
                                ;; ((exponent-marker-p c))
                                (t c))
              out))
    out))

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
    (cerror "Just return." "NIL instead of a char at token-reader.")
    (return-from token-reader (values)))
  (multiple-value-bind (package-token
                        name-token
                        saw-escape-p
                        package-markers-seen)
      (handler-bind ((multiple-package-marker-error (lambda (c)
                                                      (when *read-suppress*
                                                        (continue c))))
                     (trailing-package-marker-error
                      (lambda (c)
                        (if *read-suppress*
                            (return-from token-reader ())
                            (case (trailing-package-marker *readtable*)
                              (:read-form-in-package
                               (return-from token-reader
                                 (let ((p
                                        (find-package
                                         (translate-package
                                          (trailing-package-marker-error-token
                                           c)))))
                                   (unless p
                                     (error
                                      'find-package-error
                                      :package
                                      (trailing-package-marker-error-token c)
                                      :stream (stream-error-stream c)))
                                   (let ((*package* p))
                                     (read stream t nil t)))))
                              (:keyword (invoke-restart 'interpret-as-keyword))
                              ((()))
                              (t (error
                                  "Invalid trailing-package-marker value ~A"
                                  (trailing-package-marker *readtable*))))))))
        (collect-token stream char))
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
So that token-reader can convert the tokens to a symbol or
number. Keywords result in an empty string for the package
token. Signals a continuable error if an incoherent pattern of package
markers are seen. If continued, collect-token will continue reading
until it looks like the token ends and will return unspecified
results."
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
                         (cerror "Continue reading, collecting bad token."
                                 'multiple-package-marker-error :stream stream))
                       (incf package-markers-seen)
                       (when (and (peek-char () stream () ())
                                  (package-marker-p (peek-char () stream)))
                         (read-char stream)
                         (incf package-markers-seen))
                       (setq char (tokenize-read-char stream))
                       (unless char
                         (restart-case
                             (error 'trailing-package-marker-error
                                    :stream stream
                                    :token (case-convert token escaped-indices))
                           (interpret-as-keyword ()
                             (unless (= package-markers-seen 1)
                               (cerror "Continue reading, collecting bad token."
                                       'multiple-package-marker-error :stream stream))
                             (setq package-token "")
                             (loop-finish))))
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
    (declare (ignore i escaped-indices))
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
      (resolve-package-string
       package-string package *readtable* (package-resolution-strategy
                                           *readtable*))
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

(define-condition find-package-error (package-error reader-error)
  ()
  (:report (lambda (c s)
             (let ((errored-s (stream-error-stream c)))
               (format
                s "Error reading stream ~A~@
                   Package named ~A  not found."
                errored-s
                (package-error-package c))
               (print-file?-stream-info errored-s :stream s)))))

(define-condition multiple-package-marker-error (reader-error)
  ()
  (:report (lambda (c s)
             (format
              s
              "Impermissible pattern of package markers seen while reading ~A."
              (stream-error-stream c))
             (print-file?-stream-info (stream-error-stream c) :stream s))))

(define-condition trailing-package-marker-error (reader-error)
  ((token :reader trailing-package-marker-error-token :initarg :token))
  (:report (lambda (c s)
             (format
              s
              "Trailing package marker seen while reading ~A."
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
