(defpackage :dummy-bot
  (:use :cl))

(in-package :dummy-bot)

(defun run ()
  (sleep 0.1)
  (format t "bot output~%"))

(run)
