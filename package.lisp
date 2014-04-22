;;;; package.lisp

(defpackage #:lazy-susan
  (:nicknames #:ls)
  (:use #:cl)
  (:shadow :copy-readtable)
  (:export

   ;; Custom package resolutions
   #:resolve-package-string
   #:package-resolution-strategy
   ;; Package Local Nicknames (interface burgled from SBCL)
   #:package-local
   #:add-package-local-nickname
   #:remove-package-local-nickname
   #:package-local-nicknames
   ;; SPM, an alternative to Package-Local-Nicknames
   #:spm
   #:add-symbol-package-marker
   #:remove-symbol-package-marker
   #:package-symbol-markers
   ;; rt-local nicknames, another alternative to package-local
   #:rt-local
   #:add-rt-package-translation
   #:remove-rt-package-translation
   #:rt-package-translations


   ;; Synonym Symbols
   #:add-synonym-symbol
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
