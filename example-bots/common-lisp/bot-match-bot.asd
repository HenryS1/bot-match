(defsystem "bot-match-bot"
  :version "0.1.0"
  :author "Edward Steere"
  :license ""
  :depends-on ("herodotus"
               "cl-ppcre"
               "yason")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "A bot to play the bot-match."
  :in-order-to ((test-op (test-op "bot-match-bot/tests"))))

(defsystem "bot-match-bot/tests"
  :author "Edward Steere"
  :license ""
  :depends-on ("bot-match-bot"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for bot-match-bot"
  :perform (test-op (op c) (symbol-call :rove :run c)))
