(defpackage :turn-bot
  (:use :cl))

(in-package :turn-bot)

(defun run ()
  (let ((input (with-output-to-string (s)
                 (loop for line = (read-line t nil nil)
                    do (write-line line s)
                    while (listen)))))
    (format t "~a" input)
    (finish-output)
    (sleep 20)))

(run)
