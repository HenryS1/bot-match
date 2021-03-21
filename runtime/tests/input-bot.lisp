(defpackage :input-bot
  (:use :cl))

(in-package :input-bot)

(defun run ()
  (let ((result (with-output-to-string (s)
                   (loop for line = (read-line t nil nil)
                      while (and line (> (length line) 0))
                      do (write-line line s)))))
     (format t "~a" result)
     (format t "~%")
     (finish-output)
     (sleep 2)))

(run)
