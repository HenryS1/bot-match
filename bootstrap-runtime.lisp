(ql:quickload :cl-project)

(cl-project:make-project #p"runtime"
                         :author "Henry and Ed"
                         :email "henry.steere@gmail.com, edward.steere@gmail.com"
                         :license "MIT"
                         :depends-on '(:cl-arrows :anaphora))


