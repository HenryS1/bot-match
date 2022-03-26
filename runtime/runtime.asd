(defsystem "runtime"
  :version "0.1.0"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("anaphora"
               "herodotus"
               "metabang-bind"
               "docker-client"
               "alexandria"
               "cl-fad"
               "salmon"
               "cl-ppcre"
               "trivial-timeout")
  :components ((:module "src"
                :components
                ((:file "runtime"))))
  :description "A runtime for pitting bots against each other"
  :in-order-to ((test-op (test-op "runtime/tests"))))

(defsystem "runtime/tests"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("runtime"
               "trivia"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "runtime"))))
  :description "Test system for runtime"
  :perform (test-op (op c) (symbol-call :rove :run c)))
