(defpackage :turn-bot
  (:use :cl))

(in-package :turn-bot)

(defun run ()
  (let ((input (with-output-to-string (s)
                 (loop for line = (read-line t nil nil)
                    while (and line (> (length line) 0))
                    do (write-line line s)))))
    (format t "~a" input)
    (format t "~%")
    (sleep 20)))

(run)
