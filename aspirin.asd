;;;; aspirin.asd

(asdf:defsystem #:aspirin
  :serial t
  :description "A readtable which provides more flexible symbol reading"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:mcn-utils)
  :components ((:file "package")
               (:file "aspirin")))


(asdf:defsystem #:aspirin-test
  :serial t
  :depends-on (:aspirin)
  :components ((:file tests)))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :aspirin))))
  (asdf:operate 'load-op :aspirin-test)
  (funcall (intern (symbol-name :run-tests)
                   (find-package :aspirin-test))))
