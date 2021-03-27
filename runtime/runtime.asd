(defsystem "runtime"
  :version "0.1.0"
  :author "Henry and Ed"
  :license "MIT"
  :depends-on ("arrow-macros"
               "anaphora"
               "herodotus"
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
               "rove")
  :components ((:module "tests"
                :components
                ((:file "runtime"))))
  :description "Test system for runtime"
  :perform (test-op (op c) (symbol-call :rove :run c)))
