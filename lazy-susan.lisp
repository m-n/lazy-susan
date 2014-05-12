;;;; lazy-susan.lisp

;;; This file provides the implementation of package-local-nicknames,
;;; synonym-symbols, and a basic lazy-susan readtable generator
;;; rt abbreviates readtable.

(in-package #:lazy-susan)

;;;; Package Translations

(defgeneric resolve-package-string (package-string-or-nil package rt strategy)
  (:documentation
   "Translate a case converted package string to the name of a package.

The given package string is either nil or already case-converted, and,
if an empty string, translated to \"KEYWORD\". The strategy argument
is the current readtables package-resolution-strategy. To create a
custom package resolution strategy, created a method on this function
specializing on the strategy, and
use (setf (package-resolution-strategy your-readtable)
your-strategy)."))

(defmethod resolve-package-string ((str t)
                                   (pack t)
                                   (rt t)
                                   strategy)
  (restart-case
      (error "No method exists on resolve-package-string for strategy ~A" strategy)
    (set-default-strategy ()
      (resolve-package-string str pack rt (setf (package-resolution-strategy rt)
                                                'ls:package-local)))))

;;; Package Local Nicknames
;;; Inspired by or borrowed from sbcl's api

(defvar *package-translations* (make-hash-table))

(defmacro add-package-local-nickname
    (local-nickname actual-package &optional (package *package*))
  "Add a package local nickname at eval-always time."
  `(eval-always
     (pushnew (cons ',local-nickname ',actual-package)
              (gethash ,package *package-translations*)
              :test 'equal)))

(defmacro remove-package-local-nickname
    (local-nickname &optional (package *package*))
  "Remove a package local nickname at eval-always time."
  `(eval-always
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

(defmethod resolve-package-string
    (local-package-string package (rt t) (strategy (eql 'ls:package-local)))
  ;; when guards against confusing nil and "NIL"
  (when local-package-string
    (or (cdr (assoc local-package-string
                    (gethash package *package-translations*)
                    :test 'string=))
        local-package-string)))

;;; SPM: Symbol Package Markers

(defvar *package-symbol-translations* (make-hash-table)
  "HT mapping symbols to package names")

(defmacro add-symbol-package-marker (symbol package)
  "Add a reference from symbol to package for use while reading symbols.

  Exectured at eval-always time."
  `(eval-always
     (setf (gethash ,symbol *package-symbol-translations*)
           (package-string ,package))))

(defmacro remove-symbol-package-marker (symbol)
  "Remove the reference from symbol to any packages.

  Executed at eval-always time."
  `(eval-always (remhash ,symbol *package-symbol-translations*)))

(defun package-symbol-markers (package)
  "Return a list of the symbols which refer to package."
  (setq package (package-string package))
  (let (result)
    (maphash (lambda (k v)
               (when (string= package v)
                 (push k result)))
             *package-symbol-translations*)
    result))

(defmethod resolve-package-string (package-string
                                   package
                                   (rt t)
                                   (strategy (eql 'ls:spm)))
  (when package-string
    (multiple-value-bind (sym foundp) (find-symbol package-string
                                                   package)
      ;; foundp differentiates between finding no symbol and finding cl:nil
      (if foundp
          (gethash sym
                   *package-symbol-translations*
                   package-string)
          package-string))))

;;; Readtable Local package markers

(defvar *rt-package-translations* (make-hash-table)
  "HT mapping readtables to alists. The alists map nickname like names
  to the package name they refer to.")

(defmacro add-rt-package-translation (nickname package &optional (rt
                                                                  '*readtable*))
  "Add a readtable local package translation. (Like a nickname, but
only active for reading symbols.)"
  `(eval-always
     (assocf ,nickname
             (package-string ,package)
             (gethash ,rt *rt-package-translations*)
             :test #'string=)))

(defmacro remove-rt-package-translation (nickname &optional (rt '*readtable*))
  "Remove the readtable local package translation."
  (with-gensyms (grt gnickname)
    `(eval-always
       (let ((,gnickname ,nickname)
             (,grt ,rt))
         (setf (gethash ,grt *rt-package-translations*)
               (remove ,gnickname
                       (gethash ,grt *rt-package-translations*)
                       :test #'string=
                       :key #'car))))))

(defun rt-package-translations (&optional (rt *readtable*))
  "Return an alist mapping the package translation names to their real names."
  (gethash rt *rt-package-translations*))

(defmethod resolve-package-string (package-string
                                   (package t)
                                   rt
                                   (strategy (eql 'ls:rt-local)))
  (when package-string
    (or (cdr (assoc package-string
                    (gethash rt *rt-package-translations*)
                    :test 'string=))
        package-string)))

;;;; Synonym Symbols

(defvar *synonym-translations* (make-hash-table))

(defmacro add-synonym-symbol (synonym-symbol canonical-symbol
                          &optional (here *package*))
  "Set the symbol to read as another symbol at eval-always time.
  This setting is package-local."
  `(eval-always
     (pushnew (cons ',synonym-symbol ',canonical-symbol)
              (gethash ,here *synonym-translations*)
              :test 'eq :key 'car)))

(defmacro clear-synonym-symbols (&optional (package *package*))
  "Remove symbol translations from package eval-always time."
  `(eval-always
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

(defun rt (&optional (rt (load-time-value (copy-readtable nil))))
  "Return copy of ReadTable with lazy-susan features enabled. ASCII only.
  This sets non-whitespace, non-macro, visible ASCII characters
  excepting backslash, to be the lazy-susan's token-reader. Better
  solutions solicited.  It also installs our string and uninterned
  symbol readers which use our single-escapes as escapes; our rational
  reader which allows digit-separators in #B, #O, #X, and #R read
  numbers; and our open and close parenthesis readers which allow us
  to start tokens with a period without interfering with CCL's
  consing dot. The default base rt on Clozure Common Lisp includes
  CCL's default reader macros."
  (prog1 (setq rt (copy-readtable rt))
    (loop for char across
          ;; visible ASCII, less backslash. It interacted poorly with
          ;; CCL (1.9)'s character literals.
          "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~"
          unless (or (get-macro-character char rt)
                     (whitespacep char)) do
          (set-macro-character char #'token-reader t rt))
    (set-macro-character #\" #'string-reader () rt)
    (set-macro-character #\( #'list-reader () rt)
    (set-macro-character #\) #'close-paren-reader () rt)
    (set-dispatch-macro-character #\# #\: #'uninterned-symbol-reader rt)
    (set-dispatch-macro-character #\# #\b #'rational-reader rt)
    (set-dispatch-macro-character #\# #\o #'rational-reader rt)
    (set-dispatch-macro-character #\# #\x #'rational-reader rt)
    (set-dispatch-macro-character #\# #\r #'rational-reader rt)))

(defun string-reader (stream char &optional count)
  (declare (ignore count))
  (let ((end-char (closer char)))
    (with-output-to-string (s)
      (loop for c = (read-char stream t t t)
            until (char= c end-char) do
            (write-char (if (single-escape-p c)
                            (read-char stream t t t)
                            c)
                        s)))))

(defun rational-reader (stream char &optional count)
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
          (unless (rationalp number)
            (error 'not-rational-error
                   :symbol token
                   :base *read-base*
                   :stream stream))
          number)))))

(defun uninterned-symbol-reader (stream char &optional count)
  (declare (ignore char))
  (multiple-value-bind (package-token name-token saw-escape-p package-markers-seen)
      (collect-token stream (read-char stream t t t))
    (declare (ignore saw-escape-p package-token))
    (when (or count (plusp package-markers-seen))
      (error 'reader-error :stream stream))
    (make-symbol name-token)))

(defstruct (tconc (:type list)
                  (:constructor make-tconc
                                (list &aux (tail (last list)))))
  list
  tail)

(defun tconc-enqueue (item tconc)
  (setf (tconc-tail tconc)
        (setf (cdr (tconc-tail tconc)) item)))

(defun list-reader (stream char &optional count)
  (declare (ignore char count))
  (let (tconc)
    (labels ((peek-nonwhite ()
               (do ((char (peek-char () stream t) (peek-char () stream t)))
                   ((not (whitespacep char))
                    char)
                 (read-char stream t t t)))
             (start ()
               (let ((char (peek-nonwhite)))
                 (case char
                   (#\. (read-char stream)
                        (let ((next (peek-char () stream t t t)))
                          (cond ((whitespacep next)
                                 (error "Incorrect dot context"))
                                (t
                                 (unread-char char stream)
                                 (setq tconc (make-tconc
                                              (list (read-one t))))
                                 #'more))))
                   (#\) (read-char stream t t t)
                        (return-from list-reader ()))
                   (t (setq tconc (make-tconc (list (read-one ()))))
                      #'more))))
             (more ()
               (let ((char (peek-nonwhite)))
                 (case char
                   (#\. (read-char stream)
                        (let ((next (peek-char () stream t t t)))
                          (cond ((whitespacep next)
                                 (tconc-enqueue (read-one t) tconc)
                                 (assert (eq (read stream t t t) +cp+))
                                 (return-from list-reader
                                   (tconc-list tconc)))
                                (t
                                 (unread-char char stream)
                                 (tconc-enqueue
                                  (list (read-one ())) tconc)
                                 #'more))))
                   (#\) (read-char stream t t t)
                        (return-from list-reader (tconc-list tconc)))
                   (t (tconc-enqueue (list (read-one ())) tconc)
                      #'more))))
             (read-one (form-required)
               (let ((form (read stream t t t)))
                 (if (eq form +cp+)
                     (if form-required
                         (error "Incorrect dot context")
                         (return-from list-reader
                           (when tconc (tconc-list tconc))))
                     form))))
      (catch 'list-reader
        (loop with state = #'start
              do (setf state (funcall state)))))))

(defvar +cp+ '#:close-paren)

(defun close-paren-reader (stream char)
  (declare (ignore char stream))
  (if *read-suppress*
      ;; The close-paren-reader is required because a form such as
      ;; (#+nil foo) won't return control to the list-reader between
      ;; reading #+ and attempting to read the close paren. In a form
      ;; like #+nil(#+nil foo), the list-reader sees nil instead of
      ;; +CP+ as the value of the close paren due to *read-suppress*,
      ;; so we use throw/catch to finish reading the s-exp.
      (throw 'list-reader ())
      +cp+))

;;;; Convenient way to use a rt

(defmacro in-package/rt (package-designator &optional rt)
  "IN-PACKAGE alternative. Also sets *readtable*.
  Default RT set by (setf package-rt)."
  (with-gensyms (r p)
    `(eval-always
       (let  ((,r ,rt)
              (,p ,(package-string package-designator)))
         (unless ,r (setq ,r (package-rt ,p)))
         (prog1 (setq *package* (find-package ,p))
           (setq *readtable* ,r))))))

(defvar *package-rts* (make-hash-table :test 'equal))

(defun package-rt (package-designator)
  "Return the packages default readtable.

This could be one of three values, in order of preference:

  1. The LS default as set by (setf package-rt) or setup-package-rt
  2. The associated readtable in swank:*readtable-alist*
  3. The value of *readtable*"
  (gethash (find-package package-designator)
           *package-rts*
           (or (when (find-package "SWANK")
                 (cdr (assoc (package-string package-designator)
                             (symbol-value (find-symbol "*READTABLE-ALIST*" "SWANK"))
                             :test #'string=)))
               *readtable*)))

(defsetf package-rt (package-designator) (rt)
  "Set a package's default readtable for use with in-package/rt.

Also associates the readtable with the package in slime if swank is loaded."
  `(prog1 (setf (gethash (find-package ,package-designator) *package-rts*)
                ,rt)
     (register-rt-swank ,rt (package-string ,package-designator))))

(defun register-rt-swank (rt package-designator)
  (when (find-package "SWANK")
    (assocf package-designator rt
            (symbol-value (find-symbol "*READTABLE-ALIST*" "SWANK"))
            :test #'string=)))

(defmacro setup-package-rt ((package-designator &optional (rt-form '(ls:rt)))
                                                &body chars-functions)
  "Set package's default *readtable* to a modified copy of rt-form.

  rt-form should be a form which evaluates to a readtable, but not a
  readatbe, because readtables are not dumpable.

  For use with in-package/rt.

  The chars-functions are alternating forms, the chars to be set as
  macro characters for the functions.

  Example call:

      (setup-package-rt (my-package (ls:rt))
        (#\# #\;) 'comment-form
        #\!       'not-reader
        ls:digit-separators '(#\_))

  Sets #\! to a nonterminating macro character which uses the
  hypothetical not-reader function -- which might read as (not
  form). Use (#\!) to set it to a terminating macro character, and
  sets #; to the hypothetical dispatching macro character for
  commenting the following form.

  If SWANK is present when this is form executed, it will associate
  the package with the new readtable in SWANK:*READTABLE-ALIST*."
  (let ((package-string (package-string package-designator)))
    `(eval-always
       (setf (package-rt ,package-string)
             (copy-readtable ,rt-form))
       (in-package/rt ,package-string)
       ,@(loop for (chars function) on chars-functions by #'cddr
               collect
               (cond ((and (consp chars) (cdr chars))
                      `(set-dispatch-macro-character ,(car chars)
                                                     ,(cadr chars)
                                                     ,function))
                     ((or (characterp chars) (listp chars))
                      `(set-macro-character
                        ,(if (consp chars)
                             (car chars)
                             chars)
                        ,function ,(not (consp chars))))
                     ((symbolp chars)
                      `(setf (,(find-symbol (symbol-name chars) :ls)
                               *readtable*)
                             ,function)))))))
