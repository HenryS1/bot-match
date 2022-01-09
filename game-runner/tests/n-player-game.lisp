(defpackage n-player-game/tests/n-player-game
  (:use :cl
        :n-player-game
        :runtime
        :alexandria
        :rove))

(in-package :n-player-game/tests/n-player-game)

(defclass test-bot (bot) 
  ((bot-input :accessor bot-input :initarg :bot-input :initform "")
   (stopped :accessor stopped :initarg :stopped :initform nil)
   (bot-status :accessor bot-status :initarg :bot-status :initform :running)
   (turn-time-limit :accessor turn-time-limit :initarg :turn-time-limit)))

(defclass test-game ()
  ((turns-remaining :accessor turns-remaining :initarg :turns-remaining :initform 1)
   (moves :accessor moves :initarg :moves :initform nil)))

(defmethod get-players-input-for-turn ((game test-game))
  (list (cons "player1" "input1")))

(defmethod get-bot-input ((game test-game))
  (bot-input game))

(defmethod bot-output ((bot test-bot) time-limit) "output")

(defmethod stop-bot ((bot test-bot))
  (setf (stopped bot) t))

(defmethod interrupt-bot ((bot test-bot))
  (setf (bot-status bot) :exited))

(defmethod bot-turn ((bot test-bot) input turn-time-limit)
  (progn (setf (bot-input bot) input)
         (bot-output bot turn-time-limit)))

(defmethod advance-turn (player-moves (game test-game))
  (make-instance 'test-game 
                 :turns-remaining (- (turns-remaining game) 1)
                 :moves (append player-moves (moves game))))

(defmethod turn-time-limit ((game test-game)) 1)

(defun initialise-bots ()
  (alist-hash-table
   (list (cons "player1" (make-instance 'test-bot :bot-id "test" :bot-name "test-name")))
   :test 'equal))

(defmethod is-finished? ((game test-game))
  (= (turns-remaining game) 0))

(deftest tick
  (testing "should update the state of the game with bot output"
    (let ((game (make-instance 'test-game))
          (bots (initialise-bots)))
      (let ((next-game (tick bots game)))
        (ok (equal (moves next-game) (list (cons "player1" "output"))))
        (ok (equal (turns-remaining next-game) 0))))))

(deftest n-player-game
  (testing "should run the game until finished"
    (let ((game (make-instance 'test-game :turns-remaining 2))
          (bots (initialise-bots)))
      (let ((end-game (n-player-game bots game 0)))
        (ok (is-finished? end-game))))))
