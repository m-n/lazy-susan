;;;; package.lisp

(defpackage #:lazy-susan
  (:use #:cl #:mcn-utils)
  (:export #:token-reader
           #:read-token

           ;; Package Local Nicknames
           #:package-local-nickname
           #:remove-package-local-nickname
           #:package-local-nicknames

           ;; Synonym Symbols
           #:synonym-symbol
           #:remove-synonym-symbol

           ;; Constituent traits
           #:*decimal-point*
           #:*digit-seperator*
           #:*exponent-marker*
           #:*minus-sign*
           #:*multiple-escape*
           #:*package-marker*
           #:*plus-sign*
           #:*ratio-marker*
           #:*single-escape*
           #:*whitespace*
           ))

