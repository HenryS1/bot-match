(defpackage :guessing-game
  (:use :cl :runtime :n-player-game :alexandria :rove))

(in-package :guessing-game)

(defclass guessing-game ()
  ((numbers :accessor numbers :initarg :numbers :initform (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
   (player-guesses :accessor player-guesses :initarg :player-guesses :initform nil)
   (current-player-id :accessor current-player-id :initarg :current-player-id :initform "player1")
   (other-player-id :accessor other-player-id :initarg :other-player-id :initform "player2")
   (player-scores :accessor player-scores :initarg :player-scores :initform nil)))

(defmethod is-finished? ((game guessing-game))
  (null (numbers game)))

(defmethod game-result ((game guessing-game))
  (player-scores game))

(defmethod get-players-input-for-turn ((game guessing-game))
  (list (cons (current-player-id game) (format nil "~a~%" (car (numbers game))))))

(defmethod turn-time-limit ((game guessing-game)) 1)

(defun add-guess (old-guesses player-move)
  (let ((n (parse-integer (car (cdr player-move))))
        (guesses-for-player (cdr (assoc (car player-move) old-guesses :test 'equal))))
    (cons (cons (car player-move) (cons n guesses-for-player)) 
          (remove (car player-move) old-guesses :key #'car :test 'equal))))

(defun update-score (game)
  (lambda (old-scores player-move)
    (let* ((n (parse-integer (car (cdr player-move))))
           (target (+ (car (numbers game)) 1))
           (old-score (or (cdr (assoc (car player-move) old-scores :test 'equal)) 0))
           (new-score (+ old-score (- 1 (abs (- n target))))))
      (cons (cons (car player-move) new-score) 
            (remove (car player-move) old-scores :key #'car :test 'equal)))))

(defmethod input-parser ((game guessing-game)) #'read-output)

(defmethod advance-turn (player-moves game)
  (make-instance 'guessing-game 
                 :numbers (cdr (numbers game))
                 :player-guesses (reduce #'add-guess player-moves :initial-value (player-guesses game))
                 :current-player-id (other-player-id game)
                 :other-player-id (current-player-id game)
                 :player-scores (reduce (update-score game) player-moves :initial-value (player-scores game))))

(defparameter *base-path* (directory-namestring #.*compile-file-truename*))

(defun run-bots ()
  (loop for i from 1 to 2
     for bot-base-path in (mapcar (lambda (dir) (merge-pathnames dir *base-path* ))
                                  '("bot1/" "bot2/"))
     for definition = (read-bot-definition (merge-pathnames "definition.json" bot-base-path))
     collect (start-bot-from-definition definition (format nil "~a" bot-base-path))))

(defun run-guessing-game ()
  (format t "running guessing game~%")
  (let ((*bot-initialisation-time* 1))
   (let* ((bots (alist-hash-table (pairlis '("player1" "player2") (run-bots)) :test 'equal))
          (game (make-instance 'guessing-game)))
     (n-player-game bots game 1))))

(deftest guessing-game 
  (testing "should produce scores for bots"
    (let ((end-state (run-guessing-game)))
      (ok (equal (assoc "player1" (player-scores end-state) :test 'equal)
                 (cons "player1" 0)))
      (ok (equal (assoc "player2" (player-scores end-state) :test 'equal)
                 (cons "player2" 10))))))
