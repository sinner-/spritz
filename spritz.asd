;;;; spritz.asd

(asdf:defsystem #:spritz
  :description "Spritz stream hash."
  :version "0.0.1"
  :defsystem-depends-on (:asdf)
  :author "Russell Sim <russell.sim@gmail.com>"
  :licence "GPLv3"
  :components ((:file "spritz")))


(asdf:defsystem #:spritz/tests
  :defsystem-depends-on (:asdf)
  :depends-on (#:spritz #:FiveAM)
  :version "0.0.1"
  :licence "GPLv3"
  :pathname "tests/"
  :components ((:file "spritz_lisp_test"))
  :in-order-to ((compile-op (load-op :spritz))))


(defmethod perform ((op asdf:test-op) (system (eql (find-system :spritz))))
  (asdf:oos 'asdf:load-op :spritz/tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM))
           :spritz))
