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

(defmethod game-state ((game test-game))
  (format nil "Turns remaining ~a" (turns-remaining game)))

(defmethod get-players-input-for-turn ((game test-game))
  (list (cons "player1" (format nil "~a" (turns-remaining game)))))

(defmethod input-parser ((game test-game)) nil)

(defmethod get-bot-input ((game test-game))
  (bot-input game))

(defmethod bot-output ((bot test-bot) time-limit &optional (parser nil))
  (declare (ignore parser))
  (make-bot-turn-result :output (format nil "output ~a" (- 3 (parse-integer (bot-input bot)))) 
                        :logs (list (format nil "bot turn ~a" 
                                            (- 3 (parse-integer (bot-input bot)))))))

(defmethod stop-bot ((bot test-bot))
  (setf (stopped bot) t))

(defmethod interrupt-bot ((bot test-bot))
  (setf (bot-status bot) :exited))

(defmethod bot-turn ((bot test-bot) input turn-time-limit &optional (parser nil))
  (declare (ignore parser))
  (progn (setf (bot-input bot) input)
         (bot-output bot turn-time-limit)))

(defmethod advance-turn (player-moves (game test-game))
  (make-game-turn-result 
   :game (make-instance 'test-game 
                  :turns-remaining (- (turns-remaining game) 1)
                  :moves (append player-moves (moves game)))
   :move-log player-moves))

(defmethod turn-time-limit ((game test-game)) 1)

(defun initialise-bots ()
  (alist-hash-table
   (list (cons "player1" (make-instance 'test-bot 
                                        :bot-id "test" 
                                        :bot-name "test-name"
                                        :bot-definition nil)))
   :test 'equal))

(defmethod is-finished? ((game test-game))
  (= (turns-remaining game) 0))

(deftest tick
  (testing "should update the state of the game with bot output"
    (let ((game (make-instance 'test-game :turns-remaining 2))
          (bots (initialise-bots))
          (logging-config (make-logging-config :turns *standard-output*
                                               :moves *standard-output*
                                               :states *standard-output*)))
      (let ((next-game (tick bots game logging-config)))
        (ok (equal (moves next-game) (list (cons "player1" "output 1"))))
        (ok (equal (turns-remaining next-game) 1))))))

(deftest n-player-game
  (testing "should run the game until finished"
    (let ((game (make-instance 'test-game :turns-remaining 2))
          (bots (initialise-bots))
          (logging-config (make-logging-config :turns *standard-output*
                                               :moves *standard-output*
                                               :states *standard-output*)))
      (let ((end-game (n-player-game bots game 0 logging-config)))
        (ok (is-finished? end-game)))))
  (testing "should log the game state at each turn"
    (let ((state-logs (with-output-to-string (turn-log)
             (let ((game (make-instance 'test-game :turns-remaining 2))
                   (bots (initialise-bots))
                   (logging-config (make-logging-config :turns nil
                                                        :moves nil
                                                        :states turn-log)))
               (n-player-game bots game 0 logging-config)))))
      (ok (equalp state-logs (format nil "Turns remaining 1~%Turns remaining 0~%")))))
  (testing "should log the moves at each turn"
    (let ((move-logs (with-output-to-string (move-log)
             (let ((game (make-instance 'test-game :turns-remaining 2))
                   (bots (initialise-bots))
                   (logging-config (make-logging-config :turns nil
                                                        :moves move-log
                                                        :states nil)))
               (n-player-game bots game 0 logging-config)))))
      (ok (equalp move-logs (format nil "(player1 . output 1)~%(player1 . output 2)~%")))))
  (testing "should log output from bot turns"
    (let ((turn-logs (with-output-to-string (turn-log)
             (let ((game (make-instance 'test-game :turns-remaining 2))
                   (bots (initialise-bots))
                   (logging-config (make-logging-config :turns turn-log
                                                        :moves nil
                                                        :states nil)))
               (n-player-game bots game 0 logging-config)))))
      (ok (equalp turn-logs (format nil "bot turn 1~%bot turn 2~%"))))))
