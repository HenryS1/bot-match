(defpackage runtime
  (:use :cl :sb-gray :uiop :cl-arrows)
  (:export :run-bot :bot-output))

(in-package :runtime)

(defun to-string (bot-stream)
  (loop for c = (read-char bot-stream nil nil)
        while c
        collect c into result
        finally (return (map 'string #'identity result))))

(defun run-bot (command args)
  (launch-program (cons command args) :output :stream))

(defun bot-output (bot)
  (-> (process-info-output bot)
      (to-string))) 
