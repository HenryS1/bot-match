(defpackage :turn-bot
  (:use :cl))

(in-package :turn-bot)

(defun run ()
  (format t "Ready~%")
  (let ((input (with-output-to-string (s)
                 (loop for line = (read-line t nil nil)
                    do (write-line line s)
                    while (listen)))))
    (format t "~a" input)
    (finish-output)
    (sleep 20)))

(defun main (&rest argv)
  "Start the bot."
  (declare (ignorable argv))
  (run))
