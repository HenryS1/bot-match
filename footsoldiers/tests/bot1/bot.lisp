(defpackage :bot1
  (:use :cl))

(in-package :bot1)

(defun read-input ()
  (loop for line = (read-line t nil nil)
     while (and line (> (length line) 0))
     collect line into lines
     finally (return lines)))

(defun run ()
  (loop for input = (read-input)
     do (format t "NO-OP~%~%")))

(defun main (&rest argv)
  (declare (ignorable argv))
  (run))
