(ql:quickload :cl-project)

(cl-project:make-project #p"runtime"
                         :author "Henry and Ed"
                         :license "MIT"
                         :depends-on '(:anaphora))


