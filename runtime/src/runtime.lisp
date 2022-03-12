(defpackage runtime
  (:use :cl :arrow-macros :trivial-timeout
        :cl-ppcre
        :monad
        :either
        :alexandria
        :bind
        :docker-client
        :herodotus)
  (:export :run-bot
           :bot-output
           :has-exited
           :bot-process
           :filename
           :pause-bot
           :continue-bot
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
           :bot-restarts
           :disqualified
           :name
           :process-bot-error-output))

(in-package :runtime)

(defclass bot ()
  ((bot-id :accessor bot-id :initarg :bot-id :initform (error "bot id must be provided"))
   (bot-name :accessor bot-name :initarg :bot-name :initform (error "bot name must be provided"))))

(defclass concrete-bot (bot)
  ((bot-process :accessor bot-process :initarg :bot-process)
   (bot-definition :accessor bot-definition :initarg :bot-definition :initform (error "bot definition must be provided"))
   (command :accessor command :initarg :command :initform nil)
   (bot-restarts :accessor bot-restarts :initarg :bot-restarts :initform 0)
   (error-stream :accessor error-stream :initarg :error-stream :initform *error-output*)))

(define-json-model bot-definition (name command filename) :kebab-case)

(defun read-bot-definition (path)
  (with-open-file (f path)
    (bot-definition-json:from-json f)))

(defparameter *bot-initialisation-time* 0.5)
(defparameter *memory-limit* 2000000)
(defparameter *max-bot-restarts* 4)

(defun random-id (len)
  (map 'string #'code-char (loop for i from 1 to len collecting (+ 97 (random 26)))))

(defun run-bot (identifier)
  (let ((result (mdo (_ (stop-container identifier :kill-wait 1))
                     (attached-container (attach-container identifier))
                     (_ (start-container identifier))
                     (yield attached-container))))
    (right-value result)))

(defmethod initialise-bot ((bot concrete-bot) log-stream)
  (let* ((bot-id (random-id 10))
         (bot-name (bot-name bot))
         (initialised-bot (make-instance 'concrete-bot
                                         :bot-process (run-bot (bot-name bot))
                                         :bot-id bot-id
                                         :bot-name bot-name
                                         :bot-definition (bot-definition bot)
                                         :command (command bot)
                                         :bot-restarts (+ (bot-restarts bot) 1)
                                         :error-stream (error-stream bot))))
    (wait-for-bot-to-be-ready initialised-bot log-stream)))

(defmethod wait-for-bot-to-be-ready ((bot concrete-bot) log-stream)
  (handler-case 
      (with-timeout (*bot-initialisation-time*)
        (let ((ready-signal (read-line (container-stdout (bot-process bot)))))
          (when (or (not ready-signal) (not (equal ready-signal "Ready")))
              (format 
               log-stream
               "Got unexpected output '~a' from bot ~a while waiting for readiness signal~%" 
               ready-signal (bot-name bot))
              (kill-bot bot))
          bot))
    (timeout-error (e)
      (declare (ignore e))
      (kill-bot bot)
      (format log-stream "Timed out waiting for bot ~a to signal readiness" (bot-name bot))
      bot)))

(defmethod start-bot-from-definition ((bot-definition bot-definition) base-path 
                                      log-stream
                                      error-stream
                                      &key (memory-limit *memory-limit*)
                                        (allowed-commands nil))
  (declare (ignore memory-limit allowed-commands))
  (format log-stream "starting bot ~a~%" (name bot-definition))
  (let* ((bot-id (random-id 1000))
         (bot-name (name bot-definition))
         (bot (make-instance
               'concrete-bot
               :bot-process (run-bot bot-name)
               :bot-id bot-id
               :bot-name bot-name
               :bot-definition bot-definition
               :error-stream error-stream)))
    (wait-for-bot-to-be-ready bot log-stream)
    (pause-bot bot)
    (right bot)))

(defgeneric bot-status (bot))
(defgeneric bot-turn (bot input time-limit &optional log-stream parser))
(defgeneric disqualified (bot))

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
      nil)
    (error (e)
      (declare (ignore e))
      (format log-stream "Error while reading output from bot ~a~%" bot-name)
      nil)))

(defmethod process-bot-error-output ((bot concrete-bot) log-stream)
  (handler-case 
      (loop while (listen (container-stderr (bot-process bot)))
         for c = (read-char (container-stderr (bot-process bot)))
         do (write-char c (error-stream bot)))
    (error () (format log-stream "Error reading from bot error stream for bot ~a~%" 
                      (bot-name bot)))))

(defmethod bot-output ((bot concrete-bot) time-limit log-stream &optional (parser #'read-output))
  (-> (container-stdout (bot-process bot))
      (to-string (bot-name bot) parser time-limit log-stream)))

;; SBCL process statuses are :running :stopped :exited :signaled
(defmethod pause-bot ((bot concrete-bot)) (pause-container (bot-name bot)))

(defmethod continue-bot ((bot concrete-bot) log-stream)
  (let* ((running-bot (if (not (running (bot-status bot)))
                          (if (< (bot-restarts bot) *max-bot-restarts*)
                              (progn (format log-stream 
                                             "bot ~a not running. re-initialising it~%"
                                             (bot-name bot))
                                     (initialise-bot bot log-stream))
                             (progn 
                               (format 
                                log-stream 
                                "bot ~a not running, but has restarted the maximum number of times~%"
                                (bot-name bot))
                               bot))
                         bot)))
   (unpause-container (bot-name running-bot))
   running-bot))

(defmethod kill-bot ((bot concrete-bot))
  (unwind-protect
       (stop-container (bot-name bot) :kill-wait 0)
    (detach (bot-process bot))))

(defmethod interrupt-bot ((bot concrete-bot))
  (unwind-protect (stop-container (bot-name bot) :kill-wait 1)
    (detach (bot-process bot))))

(defmethod bot-status ((bot concrete-bot))
  (state (right-value (inspect-container (bot-name bot)))))

(defmethod send-input-to-bot ((bot concrete-bot) str)
  (with-slots (bot-process) bot
    (write-string str (container-input-stream bot-process))
    (finish-output (container-input-stream bot-process))))


(defmethod end-bot-turn ((bot concrete-bot) log-stream &optional (wait-time 0.01))
  (declare (ignore wait-time))
  (pause-bot bot)
  (when (not (paused (bot-status bot)))    
    (format log-stream "Bot ~a didn't respond to pause signal, terminating it.~%" (bot-name bot))
    (kill-bot bot)))

(defstruct bot-turn-result updated-bot output)

(defun has-exited (bot)
  (not (running (bot-status bot))))

(defmethod disqualified ((bot concrete-bot))
  (and (has-exited bot)
       (>= (bot-restarts bot) *max-bot-restarts*)))

(defmethod bot-turn ((bot concrete-bot) turn-input time-limit 
                     &optional (log-stream *standard-output*) 
                       (parser #'read-output))
  (handler-case (let ((running-bot (continue-bot bot log-stream)))
                  (if (not (has-exited running-bot))
                      (progn 
                        (send-input-to-bot running-bot turn-input)
                        (bind ((output (bot-output running-bot time-limit log-stream parser)))
                          (end-bot-turn running-bot log-stream)
                          (process-bot-error-output running-bot log-stream)
                          (make-bot-turn-result :updated-bot running-bot
                                                :output output)))
                      (make-bot-turn-result :updated-bot running-bot :output nil)))
    (error (e) (progn (format t "Error while handling turn for bot ~a: ~a~%" (bot-name bot) e)
                      (make-bot-turn-result :updated-bot bot :output nil)))))
