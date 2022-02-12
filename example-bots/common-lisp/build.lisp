(push (merge-pathnames "") ql:*local-project-directories*)
(asdf:load-system :bot-match-bot)
(ql:quickload "bot-match-bot")
(sb-ext:save-lisp-and-die #p"bot"
                          :compression 9
                          :save-runtime-options t
                          :toplevel #'bot-match-bot:main
                          :executable t)
