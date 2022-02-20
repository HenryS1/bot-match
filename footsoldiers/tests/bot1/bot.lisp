(defpackage :bot1
  (:use :cl :herodotus))

(in-package :bot1)

(define-json-model input (you))

(defun read-input ()
  (loop for input = (input-json:from-json *standard-input*)
     while (listen)
     finally (return input)))

(defun run ()
  (format t "Ready~%")
  (loop for input = (read-input)
     do (format t "No-op~%")))

(defun main (&rest argv)
  (declare (ignorable argv))
  (run))
