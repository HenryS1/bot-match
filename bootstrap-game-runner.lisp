(ql:quickload :cl-project)

(cl-project:make-project #p"game-runner"
                         :author "Henry and Ed"
                         :license "MIT"
                         :depends-on '(:arrow-macros :anaphora))
