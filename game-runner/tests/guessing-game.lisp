(defpackage :guessing-game
  (:use :cl :runtime :n-player-game :alexandria :rove))

(in-package :guessing-game)

(defclass guessing-game-state (concrete-game)
  ((turns-remaining :accessor turns-remaining :initarg :turns-remaining :initform 10)
   (numbers :accessor numbers :initform (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
   (bot-guesses :accessor bot-guesses :initform (make-hash-table :test 'equal))
   (bot-scores :accessor bot-scores :initform (make-hash-table :test 'equal))))

(defmethod is-finished? ((game guessing-game-state))
  (= (turns-remaining game) 0))

(defmethod get-bot-input ((game guessing-game-state)) (format nil "~a" (car (numbers game))))

(defmethod update-game-state ((game guessing-game-state) bot-output bot-id)
  (when bot-output
    (let ((n (parse-integer (car bot-output) :junk-allowed t)))
      (setf (gethash bot-id (bot-guesses game)) n)
      (when n
        (let ((chosen (+ (car (numbers game)) 1)))
          (setf (gethash bot-id (bot-scores game))
                (+ (- 1 (abs (- n chosen))) 
                   (gethash bot-id (bot-scores game) 0)))))))
  (pop (numbers game)))

(defmethod update-game-turn ((game guessing-game-state)) 
  (format t "turns remaining ~a~%" (turns-remaining game))
  (decf (turns-remaining game)))

(defparameter *base-path* (directory-namestring #.*compile-file-truename*))

(defun run-bots ()
  (loop for i from 1 to 2
     for bot-base-path in (mapcar (lambda (dir) (merge-pathnames dir *base-path* ))
                                  '("bot1/" "bot2/"))
     for definition = (read-bot-definition (merge-pathnames "definition.json" bot-base-path))
     collect (start-bot-from-definition definition (format nil "~a" bot-base-path))))

(defun run-guessing-game ()
  (format t "running guessing game~%")
  (let ((*bot-initialisation-time* 0.2))
   (let* ((game (make-instance 'guessing-game-state :bots (run-bots) :turn-time-limit 0.1)))
     (n-player-game game))))

(deftest guessing-game 
  (testing "should produce scores for bots"
    (let ((end-state (run-guessing-game)))
      (format t "bot scores ~a~%" (hash-table-alist (bot-scores end-state)))
      (ok (equalp (bot-scores end-state)
                  (alist-hash-table
                   (list (cons (bot-id (find-if (lambda (b)
                                                  (string= (bot-name b) "bot1"))
                                                (bots end-state))) 0)
                         (cons (bot-id (find-if (lambda (b)
                                                  (string= (bot-name b) "bot2"))
                                                (bots end-state))) 10)) :test 'equal))))))
