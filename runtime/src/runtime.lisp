(defpackage runtime
  (:use :cl :sb-gray :uiop :cl-arrows)
  (:export :run-bot :bot-output :stop-bot))

(in-package :runtime)

(defun to-string (bot-stream)
  (loop for c = (read-char bot-stream nil nil)
        while c
        collect c into result
        finally (return (map 'string #'identity result))))

(defun run-bot (command args)
  (sb-ext:run-program command args :output :stream :search t))

(defun bot-output (bot)
  (-> (sb-ext:process-output bot)
      (to-string))) 

