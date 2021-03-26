(defpackage n-player-game/tests/n-player-game
  (:use :cl
        :n-player-game
        :runtime
        :rove))
(in-package :n-player-game/tests/n-player-game)

;; NOTE: To run this test file, execute `(asdf:test-system :game-runner)' in your Lisp.

(defclass test-bot (bot) 
  ((bot-input :accessor bot-input :initarg :bot-input :initform "")
   (stopped :accessor stopped :initarg :stopped :initform nil)
   (score :accessor score :initarg :score :initform 10)))

(defclass test-game-state (game-state)
  ((finished :accessor finished :initarg :finished :initform nil)
   (bot-output :accessor bot-output :initform nil)))

(defmethod stop-bot ((bot test-bot))
  (setf (stopped bot) t))

(defmethod terminate-bots ((game-state test-game-state))
  (loop for bot in (bots game-state)
     do (stop-bot bot)))

(defmethod update-game-state ((game test-game-state) bot-output)
  (progn (push bot-output (bot-output game))
         game))

(defmethod bot-scores ((game-state test-game-state))
  (mapcar #'score (bots game-state)))

(deftest finished-game
  (testing "should terminate bots"
    (let ((game (make-instance 'test-game-state :finished t
                               :bots (list (make-instance 'test-bot)))))
      (n-player-game game)
      (ok (every (lambda (bot) (stopped bot)) (bots game))))))


