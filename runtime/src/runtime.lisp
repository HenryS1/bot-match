(defpackage runtime
  (:use :cl :uiop :arrow-macros :trivial-timeout)
  (:export :run-bot
           :bot-output
           :stop-bot
           :continue-bot
           :bot-status
           :kill-bot
           :interrupt-bot
           :send-input-to-bot
           :end-bot-turn
           :concrete-bot
           :bot
           :bot-turn))

(in-package :runtime)

(defclass bot () ())

(defclass concrete-bot (bot)
  ((bot-process :accessor bot-process :initarg :bot-process)))

(defgeneric bot-status (bot))
(defgeneric bot-turn (bot input time-limit))

(defparameter *read-output-timeout* 0.3)

(defun to-string (bot-stream)
  (handler-case 
      (with-timeout (*read-output-timeout*)
        (loop for line = (read-line bot-stream nil nil)
           while (and line (> (length line) 0))
           collect line))
    (timeout-error (e)
      (format t "timed out waiting for bot output~%")
      nil)))

(defun run-bot (command args)
  (sb-ext:run-program command args :wait nil :input :stream :output :stream :search t))

(defmethod bot-output ((bot concrete-bot))
  (-> (sb-ext:process-output (bot-process bot))
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
(defmethod stop-bot ((bot concrete-bot))
  (sb-ext:process-kill (bot-process bot) SIGSTOP))

(defmethod continue-bot ((bot concrete-bot))
  (sb-ext:process-kill (bot-process bot) SIGCONT))

(defmethod kill-bot ((bot concrete-bot))
  (sb-ext:process-kill (bot-process bot) SIGKILL))

(defmethod interrupt-bot ((bot concrete-bot))
  (sb-ext:process-kill (bot-process bot) SIGINT))

(defmethod bot-status ((bot concrete-bot))
  (sb-ext:process-status (bot-process bot)))

;; End of input is signalled by an empty line
(defmethod send-input-to-bot ((bot concrete-bot) str)
  (with-slots (bot-process) bot
    (write-line str (sb-ext:process-input bot-process))
    (write-line "" (sb-ext:process-input bot-process))
    (finish-output (sb-ext:process-input bot-process))))

(defmethod end-bot-turn ((bot concrete-bot) &optional (wait-time 0.1))
  (stop-bot bot)
  (sleep wait-time)
  (when (equal (bot-status bot) :running)
    (kill-bot bot)))

(defmethod bot-turn ((bot concrete-bot) turn-input time-limit)
  (when (not (equal (bot-status bot) :exited))
    (send-input-to-bot bot turn-input)
    (sleep time-limit)
    (end-bot-turn bot)
    (bot-output bot)))
