(defpackage :bot1
  (:use :cl :herodotus))

(in-package :bot1)

(define-json-model input (you))

(defun read-input ()
  (loop for input = (input-json:from-json *standard-input*)
     while (listen)
     finally (return input)))

(defun run ()
  (loop for input = (read-input)
     do (format t "NO-OP~%")))

(defun main (&rest argv)
  (declare (ignorable argv))
  (run))
