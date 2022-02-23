(defpackage :input-bot
  (:use :cl))

(in-package :input-bot)

(defun run ()
  (let ((result (with-output-to-string (s)
                  (loop for line = (read-line t nil nil)              
                     do (write-line line s)
                     while (listen)))))
    (format t "~a" result)
    (finish-output)
    (sleep 2)))

(defun main (&rest argv)
  "Start the bot."
  (declare (ignorable argv))
  (run))

