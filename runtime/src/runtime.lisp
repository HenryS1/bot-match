(defpackage runtime
  (:use :cl :sb-gray :uiop :cl-arrows)
  (:export :run-bot
           :bot-output
           :stop-bot
           :continue-bot
           :bot-status
           :kill-bot
           :interrupt-bot
           :send-input-to-bot))

(in-package :runtime)

(defun to-string (bot-stream)
  (loop for c = (read-char bot-stream nil nil)
        while c
        collect c into result
        finally (return (map 'string #'identity result))))

(defun run-bot (command args)
  (sb-ext:run-program command args :wait nil :input :stream :output :stream :search t))

(defun bot-output (bot)
  (-> (sb-ext:process-output bot)
      (to-string)))

;; Interrupt process
(defconstant SIGINT 2)
;; Kill process
(defconstant SIGKILL 9)
;; Resume process
#+linux (defconstant SIGCONT 18)
#-linux (defconstant SIGCONT 19)

;; Hard pause process
#+linux (defconstant SIGSTOP 19)
#-linux (defconstant SIGSTOP 17)

;; Gentle pause process
#+linux (defconstant SIGTSTP 20)
#-linux (defconstant SIGTSTP 18)

;; SBCL process statuses are :running :stopped :exited :signaled
(defun stop-bot (bot)
  (sb-ext:process-kill bot SIGSTOP))

(defun continue-bot (bot)
  (sb-ext:process-kill bot SIGCONT))

(defun kill-bot (bot)
  (sb-ext:process-kill bot SIGKILL))

(defun interrupt-bot (bot)
  (sb-ext:process-kill bot SIGINT))

(defun bot-status (bot)
  (sb-ext:process-status bot))


;; end of input is signalled by an empty line
(defun send-input-to-bot (bot str)
  (write-line str (sb-ext:process-input bot))
  (write-line "" (sb-ext:process-input bot))
  (finish-output (sb-ext:process-input bot)))
