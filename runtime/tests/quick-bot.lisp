(defpackage :dummy-bot
  (:use :cl))

(in-package :dummy-bot)

(defun run ()
  (sleep 0.2)
  (format t "bot output~%")
  (format t "~%"))

(run)
