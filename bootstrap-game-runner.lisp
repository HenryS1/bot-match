(ql:quickload :cl-project)

(cl-project:make-project #p"game-runner"
                         :author "Henry and Ed"
                         :email "henry.steere@gmail.com, edward.steere@gmail.com"
                         :license "MIT"
                         :depends-on '(:arrow-macros :anaphora))
