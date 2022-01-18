(defpackage :dummy-bot
  (:use :cl))

(in-package :dummy-bot)

(defun run ()
  (sleep 1)
  (format t "bot output~%")
  (finish-output))

(run)
