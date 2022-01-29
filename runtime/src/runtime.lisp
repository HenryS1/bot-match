(defpackage runtime
  (:use :cl :uiop :arrow-macros :trivial-timeout
        :cl-ppcre
        :bind
        :herodotus)
  (:export :run-bot
           :bot-output
           :relative-filepath
           :stop-bot
           :continue-bot
           :name
           :command
           :bot-status
           :kill-bot
           :interrupt-bot
           :send-input-to-bot
           :end-bot-turn
           :concrete-bot
           :bot
           :bot-id
           :bot-name
           :bot-definition
           :read-bot-definition
           :start-bot-from-definition
           :bot-turn
           :*bot-initialisation-time*
           :read-output
           :make-bot-turn-result
           :bot-turn-result-output
           :bot-turn-result-logs
           :bot-turn-result-updated-bot))

(in-package :runtime)

(defclass bot ()
  ((bot-id :accessor bot-id :initarg :bot-id :initform (error "bot id must be provided"))
   (bot-name :accessor bot-name :initarg :bot-name :initform (error "bot name must be provided"))))

(defclass concrete-bot (bot)
  ((bot-process :accessor bot-process :initarg :bot-process)
   (bot-definition :accessor bot-definition :initarg :bot-definition :initform (error "bot definition must be provided"))
   (command :accessor command :initarg :command :initform (error "command must be provided"))))

(define-json-model bot-definition (name command relative-filepath) :kebab-case)

(defun read-bot-definition (path)
  (with-open-file (f path)
    (bot-definition-json:from-json f)))

(defparameter *bot-initialisation-time* 0)

(defun random-id (len)
  (map 'string #'code-char (loop for i from 1 to len collecting (+ 97 (random 26)))))

(defun run-bot (command args)
  (sb-ext:run-program command args :wait nil :input :stream :output :stream :search t))

(defmethod initialise-bot ((bot concrete-bot))
  (make-instance 'concrete-bot
                 :bot-process (run-bot "bash" (list "-c" (command bot)))
                 :bot-id (bot-id bot)
                 :bot-name (bot-name bot)
                 :bot-definition (bot-definition bot)
                 :command (command bot)))

(defparameter *memory-limit* 2000000)

(defmethod start-bot-from-definition ((bot-definition bot-definition) base-path 
                                      &optional (memory-limit *memory-limit*))
  (let* ((templated-command
          (regex-replace "<bot-file>" 
                         (command bot-definition)
                         (concatenate 'string base-path (relative-filepath bot-definition))))
         (memory-limited-command (format nil "ulimit -v ~a; ~a" 
                                         memory-limit
                                         templated-command)))
    (format t "BOT COMMAND ~a~%" memory-limited-command)
    (let ((bot (make-instance 
                'concrete-bot
                :bot-process (run-bot "bash" (list "-c" memory-limited-command))
                :bot-id (random-id 10)
                :bot-name (name bot-definition)
                :bot-definition bot-definition
                :command memory-limited-command)))      
      (when (> *bot-initialisation-time* 0)
        (sleep *bot-initialisation-time*))
      (stop-bot bot)
      bot)))

(defgeneric bot-status (bot))
(defgeneric bot-turn (bot input time-limit &optional parser))

(defun read-output (bot-stream)
  (loop for line = (read-line bot-stream nil nil)
     collect line
     while (listen bot-stream)))

(defun to-string (bot-stream bot-name parser time-limit logs)
  (handler-case 
      (with-timeout (time-limit)
        (loop for bot-output = (funcall parser bot-stream)
           while (listen bot-stream)
           finally (return (cons bot-output (cons (format nil "Bot ~a returned output ~a" bot-name bot-output) logs)))))
    (timeout-error (e)
      (declare (ignore e))
      (cons nil (cons (format nil "Timed out waiting for output from bot ~a" bot-name) logs)))))

(defmethod bot-output ((bot concrete-bot) time-limit logs &optional (parser #'read-output))
  (-> (sb-ext:process-output (bot-process bot))
      (to-string (bot-name bot) parser time-limit logs)))

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
  ;; when a bot process exits on its own it's status is :exited 
  ;; when it is killed its status is :signaled
  (let ((running-bot (if (or (equal (bot-status bot) :exited)
                             (equal (bot-status bot) :signaled))
                         (initialise-bot bot)
                         bot)))
   (sb-ext:process-kill (bot-process running-bot) SIGCONT)
   running-bot))

(defmethod kill-bot ((bot concrete-bot))
  (sb-ext:process-kill (bot-process bot) SIGKILL))

(defmethod interrupt-bot ((bot concrete-bot))
  (sb-ext:process-kill (bot-process bot) SIGINT))

(defmethod bot-status ((bot concrete-bot))
  (sb-ext:process-status (bot-process bot)))

(defmethod send-input-to-bot ((bot concrete-bot) str)
  (with-slots (bot-process) bot
    (write-string str (sb-ext:process-input bot-process))
    (finish-output (sb-ext:process-input bot-process))))

(defmethod end-bot-turn ((bot concrete-bot) logs &optional (wait-time 0.1))
  (stop-bot bot)
  (sleep wait-time)
  (when (equal (bot-status bot) :running)    
    (kill-bot bot)
    (cons (format nil "Bot ~a didn't respond to signal, terminating it." (bot-name bot)) logs)))

(defstruct bot-turn-result updated-bot output logs)

(defmethod bot-turn ((bot concrete-bot) turn-input time-limit &optional (parser #'read-output))
  (let ((running-bot (continue-bot bot)))
    (send-input-to-bot running-bot turn-input)
    (bind (((output . logs) (bot-output running-bot time-limit nil parser)))
      (make-bot-turn-result :updated-bot running-bot
                            :output output 
                            :logs (reverse (end-bot-turn running-bot logs))))))
