;;;; aspirin.asd

(asdf:defsystem #:aspirin
  :serial t
  :description "A more flexible implementation of the Common Lisp reader."
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:mcn-utils)
  :components ((:file "package")
               (:file "aspirin")))

