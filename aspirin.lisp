;;;; aspirin.lisp

(in-package #:aspirin)

(syntax-bind ()
  (#\# #\;) 'comment-line-suppress-forms)

#; Trying #'constituent-reader instead
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

(defun constituent-reader (stream char &optional count)
  (declare (ignorable stream count))
  (let ((token (make-array 1 :initial-element char :element-type 'character
			   :fill-pointer t :adjustable t)))
    (loop for c = (read-char stream)
	  until (whitespacep c)
	  do (vector-push-extend c token))
    token))

(defun whitespacep (c)
  (in c #\  #\Newline))

(defclass-autoargs aspirin-readtable ()
  (aspirin-case))
