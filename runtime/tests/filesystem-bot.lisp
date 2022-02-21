(defpackage :turn-bot
  (:use :cl))

(in-package :turn-bot)

(defun run ()
  (format t "Ready~%")
  (with-open-file (f "./test-file" 
                     :direction :output 
                     :if-exists :supersede
                     :if-does-not-exist :create)
    (write-string "hello" f)
    (finish-output f))
  (sleep 20))

(run)
