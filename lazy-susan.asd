;;;; lazy-susan.asd

(asdf:defsystem #:lazy-susan
  :serial t
  :description "A readtable which provides more flexible symbol reading"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:mcn-utils)
  :components ((:file "package")
               (:file "lazy-susan")))


(asdf:defsystem #:lazy-susan-test
  :serial t
  :depends-on (:lazy-susan)
  :components ((:file tests)))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :lazy-susan))))
  (asdf:operate 'load-op :lazy-susan-test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :lazy-susan-test))))
