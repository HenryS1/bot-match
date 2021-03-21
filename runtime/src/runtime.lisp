(defpackage runtime
  (:use :cl :external-program)
  (:export :run-bot :get-bot-output :pause-bot :kill-bot))
(in-package :runtime)

(defun run-bot (command args)
  (with-output-to-string (s)
    (run command args :output s)))

