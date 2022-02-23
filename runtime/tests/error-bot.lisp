(defpackage :error-bot
  (:use :cl))

(in-package :error-bot)

(defun run ()
  (format *error-output* "Bot error output"))

(defun main (&rest argv)
  "Start the bot."
  (declare (ignorable argv))
  (run))
