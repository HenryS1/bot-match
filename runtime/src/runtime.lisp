(defpackage runtime
  (:use :cl :arrow-macros :trivial-timeout
        :cl-ppcre
        :monad
        :either
        :alexandria
        :bind
        :herodotus)
  (:export :run-bot
           :bot-output
           :filename
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
           :bot-turn-result-updated-bot
           :*max-bot-restarts*
           :*memory-limit*
           :bot-restarts))

(in-package :runtime)

(defclass bot ()
  ((bot-id :accessor bot-id :initarg :bot-id :initform (error "bot id must be provided"))
   (bot-name :accessor bot-name :initarg :bot-name :initform (error "bot name must be provided"))))

(defclass concrete-bot (bot)
  ((bot-process :accessor bot-process :initarg :bot-process)
   (bot-definition :accessor bot-definition :initarg :bot-definition :initform (error "bot definition must be provided"))
   (command :accessor command :initarg :command :initform (error "command must be provided"))
   (bot-restarts :accessor bot-restarts :initarg :bot-restarts :initform 0)))

(define-json-model bot-definition (name command filename) :kebab-case)

(defun read-bot-definition (path)
  (with-open-file (f path)
    (bot-definition-json:from-json f)))

(defparameter *bot-initialisation-time* 0.5)
(defparameter *memory-limit* 2000000)
(defparameter *max-bot-restarts* 4)

(defun random-id (len)
  (map 'string #'code-char (loop for i from 1 to len collecting (+ 97 (random 26)))))

(defun run-bot (command args)
  (sb-ext:run-program command args :wait nil :input :stream :output :stream :search t))

(defmethod initialise-bot ((bot concrete-bot) log-stream)
  (let ((initialised-bot (make-instance 'concrete-bot
                                        :bot-process (run-bot "bash" (list "-c" (command bot)))
                                        :bot-id (bot-id bot)
                                        :bot-name (bot-name bot)
                                        :bot-definition (bot-definition bot)
                                        :command (command bot)
                                        :bot-restarts (+ (bot-restarts bot) 1))))
    (wait-for-bot-to-be-ready initialised-bot log-stream)))

(defmethod wait-for-bot-to-be-ready ((bot concrete-bot) log-stream)
  (handler-case 
      (with-timeout (*bot-initialisation-time*)
        (let ((ready-signal (read-line (sb-ext:process-output (bot-process bot)) nil nil)))
          (when (or (not ready-signal) (not (equal ready-signal "READY")))
              (format log-stream "Got unexpected output '~a' from bot ~a while waiting for readiness signal~%" ready-signal (bot-name bot))
              (kill-bot bot))
          bot))
    (timeout-error (e)
      (declare (ignore e))
      (kill-bot bot)
      (format log-stream "Timed out waiting for bot ~a to signal readiness" (bot-name bot))
      bot)))

(defparameter *default-allowed-commands* 
  (alist-hash-table (list (cons "lisp-ros" "ros +Q -- <bot-file>")) :test 'equal))

(defmethod start-bot-from-definition ((bot-definition bot-definition) base-path 
                                      log-stream
                                      &key (memory-limit *memory-limit*)
                                        (allowed-commands *default-allowed-commands*))
  (if (not (gethash (command bot-definition) allowed-commands))
      (left (format nil "Failed to start bot ~a. Unrecognised command ~a." 
                    (name bot-definition) (command bot-definition)))
      (let* ((templated-command
              (regex-replace "<bot-file>" 
                             (gethash (command bot-definition) allowed-commands)
                             (format nil "~a" (merge-pathnames
                                               (file-namestring
                                                (parse-namestring (filename bot-definition)))
                                               (cl-fad:pathname-as-directory 
                                                (parse-namestring base-path))))))
             (memory-limited-command (format nil "ulimit -v ~a; ~a" 
                                             memory-limit
                                             templated-command)))
        (format log-stream "starting bot ~a using command ~a~%" 
                (name bot-definition)
                memory-limited-command)
        (let ((bot (make-instance 
                    'concrete-bot
                    :bot-process (run-bot "bash" (list "-c" memory-limited-command))
                    :bot-id (random-id 10)
                    :bot-name (name bot-definition)
                    :bot-definition bot-definition
                    :command memory-limited-command)))
          (wait-for-bot-to-be-ready bot log-stream)
          (stop-bot bot)
          (right bot)))))

(defgeneric bot-status (bot))
(defgeneric bot-turn (bot input time-limit &optional log-stream parser))

(defun read-output (bot-stream)
  (loop for line = (read-line bot-stream nil nil)
     collect line
     while (listen bot-stream)))

(defun to-string (bot-stream bot-name parser time-limit log-stream)
  (handler-case 
      (with-timeout (time-limit)
        (loop for bot-output = (funcall parser bot-stream)
           while (listen bot-stream)
           finally (return (progn
                             (format log-stream "Bot ~a returned output ~a~%" bot-name bot-output)
                             bot-output))))
    (timeout-error (e)
      (declare (ignore e))
      (format log-stream "Timed out waiting for output from bot ~a~%" bot-name)
      nil)))

(defmethod bot-output ((bot concrete-bot) time-limit log-stream &optional (parser #'read-output))
  (-> (sb-ext:process-output (bot-process bot))
      (to-string (bot-name bot) parser time-limit log-stream)))

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

(defmethod continue-bot ((bot concrete-bot) log-stream)
  ;; when a bot process exits on its own it's status is :exited 
  ;; when it is killed its status is :signaled
  (let ((running-bot (if (or (equal (bot-status bot) :exited)
                             (equal (bot-status bot) :signaled))
                         (if (< (bot-restarts bot) *max-bot-restarts*)
                             (progn (format log-stream "bot ~a not running. re-initialising it~%"
                                            (bot-name bot))
                                    (initialise-bot bot log-stream))
                             (progn 
                               (format 
                                log-stream 
                                "bot ~a not running, but has restarted the maximum number of times~%"
                                (bot-name bot))
                               bot))
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

(defmethod end-bot-turn ((bot concrete-bot) log-stream &optional (wait-time 0.1))
  (stop-bot bot)
  (sleep wait-time)
  (when (equal (bot-status bot) :running)    
    (format log-stream "Bot ~a didn't respond to stop signal, terminating it.~%" (bot-name bot))
    (kill-bot bot)))

(defstruct bot-turn-result updated-bot output)

(defun not-exited (bot)
  (not (or (equalp (bot-status bot) :exited)
           (equalp (bot-status bot) :signaled))))

(defmethod bot-turn ((bot concrete-bot) turn-input time-limit 
                     &optional (log-stream *standard-output*) 
                       (parser #'read-output))
  (handler-case (let ((running-bot (continue-bot bot log-stream)))
                  (if (not-exited running-bot)
                      (progn 
                        (send-input-to-bot running-bot turn-input)
                        (bind ((output (bot-output running-bot time-limit log-stream parser)))
                          (end-bot-turn running-bot log-stream)
                          (make-bot-turn-result :updated-bot running-bot
                                                :output output)))
                      (make-bot-turn-result :updated-bot running-bot :output nil)))
    (error (e) (progn (format t "Error while handling turn for bot ~a: ~a~%" (bot-name bot) e)
                      (make-bot-turn-result :updated-bot bot :output nil)))))
