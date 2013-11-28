;;;; package.lisp

(defpackage #:lazy-susan
  (:use #:cl #:mcn-utils)
  (:export #:token-reader
           #:read-token
           #:package-local-nickname
           #:remove-package-local-nickname
           #:package-local-nicknames
           #:synonym-symbol
           #:remove-synonym-symbol
           ;; We export consituent trait variables from within the file
           . #.(when (find-package '#:lazy-susan)
                 (loop for s being the external-symbols of '#:lazy-susan
                       collect s))))

