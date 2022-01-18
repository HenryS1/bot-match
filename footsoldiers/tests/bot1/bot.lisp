(defpackage :bot1
  (:use :cl))

(in-package :bot1)

(defun read-input ()
  (loop for line = (read-line t nil nil)
     collect line into lines
     while (listen)
     finally (return lines)))

(defun run ()
  (loop for input = (read-input)
     do (format t "NO-OP~%")))

(defun main (&rest argv)
  (declare (ignorable argv))
  (run))
