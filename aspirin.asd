;;;; aspirin.asd

(asdf:defsystem #:aspirin
  :serial t
  :description "A readtable which provides more flexible symbol reading"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:mcn-utils)
  :components ((:file "package")
               (:file "aspirin")))

