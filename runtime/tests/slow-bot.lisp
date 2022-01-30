(defpackage :forever-bot
  (:use :cl))

(in-package :forever-bot)

(defun run ()
  (format t "READY~%")
  (sleep 20))

(run)
