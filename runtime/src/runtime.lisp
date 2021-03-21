(defpackage runtime
  (:use :cl :external-program :sb-gray)
  (:export :run-bot :get-bot-output :pause-bot :kill-bot))

(in-package :runtime)

(defclass bot-stream (fundamental-stream)
  ((input-buffer :accessor input-buffer :initarg :input-buffer :initform 
                 (make-array 5 :fill-pointer 0 :element-type 'character :adjustable t))))

(defmethod stream-read-char ((stream bot-stream))
  (with-slots (input-buffer) stream
    (if (> (fill-pointer input-buffer) 0)
        (vector-pop input-buffer)
        nil)))

(defmethod stream-element-type ((stream bot-stream)) 'character)

(defmethod stream-write-char ((stream bot-stream) c)
  (with-slots (input-buffer) stream
    (vector-push-extend c input-buffer (max 1 (length input-buffer)))))

(defmethod stream-write-string ((stream bot-stream) str &optional (start 0) (end nil))
  (loop for i from start to (or end (- (length str) 1))
     for c = (aref str i)
     do (stream-write-char stream c)))

(defun to-string (bot-stream)
  (map 'string #'identity (input-buffer bot-stream)))

(defclass bot-process ()
  ((stream :accessor :stream :initarg :stream :initform (make-instance 'bot-stream))
   (process :accessor :process :initarg :process :initform (error "Bot process expected during initialization."))))

(defun run-bot (command args)
  (let ((s (make-instance 'bot-stream)))
    (run command args :output s)
    (to-string s)))

