(defpackage :forever-bot
  (:use :cl))

(in-package :forever-bot)

(defun run ()
  (format t "Ready~%")
  (sleep 20))

(run)
