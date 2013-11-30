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
           ;; exported for their setf function
           #:decimal-points
           #:digit-seperators
           #:exponent-markers
           #:minus-signs
           #:multiple-escapes
           #:package-markers
           #:plus-signs
           #:ratio-markers
           #:single-escapes
           #:whitespaces
           ))

