(defsystem "footsoldiers"
  :version "0.1.0"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("arrow-macros"
               "anaphora"
               "metabang-bind"
               "sycamore")
  :components ((:module "src"
                :components
                ((:file "footsoldiers.lisp"))))
  :description "A game called footsoldiers"
  :in-order-to ((test-op (test-op "footsoldiers/tests"))))

(defsystem "footsoldiers/tests"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("footsoldiers"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "footsoldiers.lisp"))))
  :description "Test system for footsoldiers"
  :perform (test-op (op c) (symbol-call :rove :run c)))
