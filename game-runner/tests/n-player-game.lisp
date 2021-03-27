(defpackage n-player-game/tests/n-player-game
  (:use :cl
        :n-player-game
        :runtime
        :rove))
(in-package :n-player-game/tests/n-player-game)

(defclass test-bot (bot) 
  ((bot-input :accessor bot-input :initarg :bot-input :initform "")
   (stopped :accessor stopped :initarg :stopped :initform nil)
   (bot-status :accessor bot-status :initarg :bot-status :initform :running)
   (score :accessor score :initarg :score :initform 10)
   (bot-output :accessor bot-output :initarg :bot-output :initform nil)
   (turn-time-limit :accessor turn-time-limit :initarg :turn-time-limit)))

(defclass test-game-state (game-state)
  ((turns-remaining :accessor turns-remaining :initarg :turns-remaining)
   (bot-input :accessor bot-input :initarg :bot-input :initform nil)
   (bot-output :accessor bot-output :initform nil)))

(defmethod get-bot-input ((game test-game-state))
  (bot-input game))

(defmethod stop-bot ((bot test-bot))
  (setf (stopped bot) t))

(defmethod terminate-bots ((game-state test-game-state))
  (loop for bot in (bots game-state)
     do (stop-bot bot)))

(defmethod bot-turn (input (bot test-bot) turn-time-limit)
  (progn (setf (bot-input bot) input)
         (setf (turn-time-limit bot) turn-time-limit)
         (bot-output bot)))

(defmethod update-game-state ((game test-game-state) bot-output)
  (progn (push bot-output (bot-output game))
         game))

(defmethod update-game-turn ((game test-game-state))
  (decf (turns-remaining game))
  game)

(defmethod bot-scores ((game-state test-game-state))
  (mapcar #'score (bots game-state)))

(defmethod is-finished? ((game test-game-state))
  (= (turns-remaining game) 0))

(deftest finish-game
  (testing "should terminate bots"
    (let ((game (make-instance 'test-game-state :turns-remaining 0
                               :bots (list (make-instance 'test-bot)))))
      (finish-game game)
      (ok (every (lambda (bot) (stopped bot)) (bots game)))))
  (testing "should return the scores of each bot"
    (let ((game (make-instance 'test-game-state :turns-remaining 0
                               :bots (list (make-instance 'test-bot :score 13)
                                           (make-instance 'test-bot :score 21)))))
      (ok (equal (finish-game game) (list 13 21))))))

(deftest tick
  (testing "should update the state of the game with bot output"
    (let ((game (make-instance 'test-game-state :turns-remaining 1
                               :bots (list (make-instance 'test-bot :bot-output "output")))))
      (tick game)
      (ok (equal (bot-output game) (list "output")))))
  (testing "should give each a turn"
    (let ((game (make-instance 'test-game-state :turns-remaining 1
                               :bot-input "bot-input"
                               :bots (list (make-instance 'test-bot) (make-instance 'test-bot)))))
      (tick game)
      (ok (equal (mapcar #'bot-input (bots game)) (list "bot-input" "bot-input")))))
  (testing "should update the current turn for the game"
    (let ((game (make-instance 'test-game-state :turns-remaining 1)))
      (tick game)
      (ok (equal (turns-remaining game) 0)))))

(deftest n-player-game
  (testing "should run the game until finished"
    (let ((game (make-instance 'test-game-state :turns-remaining 2)))
      (n-player-game game)
      (ok (is-finished? game)))))