(defsystem "footsoldiers"
  :version "0.1.0"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("arrow-macros"
               "anaphora"
               "metabang-bind"
               "salmon"
               "trivia"
               "trivia.ppcre"
               "runtime"
               "game-runner")
  :components ((:module "src"
                :components
                ((:file "footsoldiers"))))
  :description "A game called footsoldiers"
  :in-order-to ((test-op (test-op "footsoldiers/tests"))))

(defsystem "footsoldiers/tests"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("footsoldiers"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "footsoldiers"))))
  :description "Test system for footsoldiers"
  :perform (test-op (op c) (symbol-call :rove :run c)))