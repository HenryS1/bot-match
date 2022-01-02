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
   (score :accessor score :initarg :score :initform 10)

   (turn-time-limit :accessor turn-time-limit :initarg :turn-time-limit)))

(defclass test-game (game)
  ((turns-remaining :accessor turns-remaining :initarg :turns-remaining :initform 1)
   (moves :accessor moves :initarg :moves :initform nil)))

(defmethod get-players-input-for-turn ((game test-game))
  (list (cons "player1" "input1")))

(defmethod get-bot-input ((game test-game))
  (bot-input game))

(defmethod stop-bot ((bot test-bot))
  (setf (stopped bot) t))

(defmethod interrupt-bot ((bot test-bot))
  (setf (bot-status bot) :exited))

(defmethod bot-turn ((bot test-bot) input turn-time-limit)
  (progn (setf (bot-input bot) input)
         (bot-output bot)))

(defmethod advance-turn (player-moves (game test-game))
  (make-instance 'test-game 
                 :turns-remaining (- (turns-remaining game) 1)
                 :moves (cons player-moves (moves game))))

(defun initialise-bots ()
  (alist-hash-table
   (list (cons "player1" . (make-instance 'test-bot :bot-output "output"
                                          :bot-id "test")))
   :test 'equal))

(defmethod game-result ((game test-game))
  (mapcar #'score (bots game)))

(defmethod is-finished? ((game test-game))
  (= (turns-remaining game) 0))

(deftest tick
  (testing "should update the state of the game with bot output"
    (let ((game (make-instance 'test-game))
          (bots (initialise-bots)))
      (tick bots game 0)
      (ok (equal (moves game) (list (cons "player1" . "output"))))
      (ok (equal (turns-remaining game) 0)))))

(deftest n-player-game
  (testing "should run the game until finished"
    (let ((game (make-instance 'test-game :turns-remaining 2)))
      (n-player-game game)
      (ok (is-finished? game)))))

