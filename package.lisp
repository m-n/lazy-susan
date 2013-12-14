;;;; package.lisp

(defpackage #:lazy-susan
  (:use #:cl)
  (:export
   ;; Package Local Nicknames (interface burgled from SBCL)
   #:package-local-nickname
   #:remove-package-local-nickname
   #:package-local-nicknames

   ;; Synonym Symbols
   #:synonym-symbol
   #:clear-synonym-symbols

   ;; Constituent traits and Syntax Types
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

   ;; Reading
   #:rt
   #:token-reader
   #:collect-token
   #:in-project
   #:project-rt
   ))

