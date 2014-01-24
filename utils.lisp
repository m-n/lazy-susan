;;;; utils.lisp
;;; General utilities

(in-package #:lazy-susan)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect
               `(,n (gensym ,(concatenate 'string (symbol-name n) "-"))))
     ,@body))

(defmacro idoveq ((index-var value-var seq &optional return) &body body)
  "indexed-do-vector"
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

(defmacro assocf (key val place &rest keyargs &environment env)
  "Sets first instance of key in alist to val, adding entry if necessary."
  (multiple-value-bind (vars vals nv set access)
      (get-setf-expansion place env)
    (with-gensyms (gkey gval cons gaccess)
      `(let ((,gkey ,key)
             (,gval ,val))
         (let* (,@(mapcar #'list vars vals)
                (,gaccess ,access)
                (,(car nv)
                 (let ((,cons (assoc ,gkey ,gaccess ,@keyargs)))
                   (if ,cons
                       (prog1 ,gaccess
                         (setf (cdr ,cons) ,gval))
                       (acons ,gkey ,gval ,gaccess)))))
           ,set)))))
