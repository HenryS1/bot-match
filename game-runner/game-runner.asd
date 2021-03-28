(defsystem "game-runner"
  :version "0.1.0"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("arrow-macros"
               "anaphora"
               "cl-ppcre"
               "runtime")
  :components ((:module "src"
                :components
                ((:file "n-player-game"))))
  :description "Toolkit for building engines to run bot matches"
  :in-order-to ((test-op (test-op "game-runner/tests"))))

(defsystem "game-runner/tests"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("game-runner"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "n-player-game")
                 (:file "guessing-game"))))
  :description "Test system for game-runner"
  :perform (test-op (op c) (symbol-call :rove :run c)))
