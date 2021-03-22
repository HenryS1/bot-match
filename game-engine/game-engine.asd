(defsystem "game-engine"
  :version "0.1.0"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("cl-arrows"
               "anaphora")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "game-engine/tests"))))

(defsystem "game-engine/tests"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("game-engine"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for game-engine"
  :perform (test-op (op c) (symbol-call :rove :run c)))
