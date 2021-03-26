(defpackage n-player-game
  (:use :cl :runtime :herodotus :cl-ppcre :alexandria)
  (:export :game-state :bot-definition :n-player-game
           :terminate-bots :bot-scores :bots :turn-time-limit))

(in-package :n-player-game)

(define-json-model bot-definition (command name))

(defmethod start-bot ((bot-definition bot-definition))
  (let ((command-parts (split "\\s+" (command bot-definition))))
    (make-instance 'concrete-bot (run-bot (car command-parts) (cdr command-parts)))))

(defclass game-state ()
  ((bots :accessor bots :initarg :bots :initform nil)
   (turn-time-limit :accessor turn-time-limit :initarg :turn-time-limit :initform 1)))

(defgeneric is-finished? (game-state))
(defgeneric next-turn (game-state))
(defgeneric terminate-bots (game-state))
(defgeneric bot-scores (game-state))
(defgeneric get-bot-input (game-state))
(defgeneric update-game-state (game-state bot-output))

(defmethod n-player-game ((game-start game-state))
  (loop for game = game-start then (update-game-state game bot-output)
     for bots = (bots game-start) then (bots game)
     for bot in bots
     for bot-output = (when (not (equal (bot-status bot) :exited))
                        (bot-turn (get-bot-input game) bot (turn-time-limit game)))
     while (not (is-finished? game))
     finally (return (progn 
                       (terminate-bots game)
                       (bot-scores game)))))
