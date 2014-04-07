;;;; package.lisp

(defpackage #:lazy-susan
  (:nicknames #:ls)
  (:use #:cl)
  (:shadow :copy-readtable)
  (:export
   ;; Package Local Nicknames (interface burgled from SBCL)
   #:add-package-local-nickname
   #:remove-package-local-nickname
   #:package-local-nicknames

   ;; Synonym Symbols
   #:synonym-symbol
   #:clear-synonym-symbols

   ;; Constituent traits and Syntax Types
   ;; exported for their setf function
   #:decimal-points
   #:digit-separators
   #:exponent-markers
   #:minus-signs
   #:multiple-escapes
   #:package-markers
   #:plus-signs
   #:ratio-markers
   #:single-escapes
   #:whitespaces

   #:trailing-package-marker

   ;; Reading
   #:rt
   #:token-reader
   #:collect-token
   #:in-package/rt
   #:package-rt
   #:setup-package-rt
   ))
