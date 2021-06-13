(defpackage n-player-game
  (:use :cl :runtime :herodotus :cl-ppcre :alexandria)
  (:export :game-state :bot-definition :n-player-game :is-finished?
           :bot-status :get-bot-input :update-game-state :update-game-turn
           :tick :finish-game :concrete-game
           :terminate-bots :bot-scores :bots :turn-time-limit))

(in-package :n-player-game)

(defclass game-state ()
  ((bots :accessor bots :initarg :bots :initform nil)
   (turn-time-limit :accessor turn-time-limit :initarg :turn-time-limit :initform 1)))

(defclass concrete-game (game-state) ())

(defgeneric is-finished? (game-state))
(defgeneric terminate-bots (game-state))
(defgeneric bot-scores (game-state))
(defgeneric get-bot-input (game-state))
(defgeneric update-game-state (game-state bot-output bot-id))
(defgeneric update-game-turn (game-state))

(defmethod terminate-bots ((game concrete-game))
  (loop for bot in (bots game) 
     do (interrupt-bot bot))
  (sleep 0.5)
  (loop for bot in (bots game)
     when (not (equal (bot-status bot) :exited))
     do (kill-bot bot)))

(defmethod finish-game ((game-state game-state))
  (progn (terminate-bots game-state)
         (bot-scores game-state)))

(defmethod tick ((game-state game-state))
  (loop for bot in (bots game-state)
     for bot-output = (bot-turn bot (get-bot-input game-state) (turn-time-limit game-state))
     do (update-game-state game-state bot-output (bot-id bot))
     finally (update-game-turn game-state)))

(defmethod n-player-game ((game-state game-state))
  (unwind-protect 
       (loop while (not (is-finished? game-state))
          do (tick game-state)
          finally (return (bot-scores game-state)))
    (terminate-bots game-state)))
