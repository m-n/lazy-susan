;;;; utils.lisp
;;; General utilities

(in-package #:lazy-susan)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect
               `(,n (gensym ,(concatenate 'string (symbol-name n) "-"))))
     ,@body))

(defmacro idoveq ((index-var value-var seq &optional return) &body body)
  (let ((length (gensym))
        (gseq (gensym)))
    `(let* ((,gseq ,seq)
	    (,length (length ,gseq))
	    ,value-var)
       (dotimes (,index-var  ,length ,return)
         (setq ,value-var (elt ,gseq ,index-var))
         ,@body))))

(defun closer (char)
  (case char
    (#\( #\))
    (#\{ #\})
    (#\[ #\])
    (#\< #\>)
    (t char)))
