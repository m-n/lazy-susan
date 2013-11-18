;;;; package.lisp

(defpackage #:lazy-susan
  (:use #:cl #:mcn-utils)
  (:export #:token-reader
           #:read-token
           #:package-local-nickname
           #:remove-package-local-nickname
           #:package-local-nicknames))

