(defpackage n-player-game
  (:use :cl :runtime :herodotus :cl-ppcre :alexandria)
  (:export :game-state :bot-definition :n-player-game :is-finished?
           :bot-status :get-bot-input :update-game-state :update-game-turn
           :terminate-bots :bot-scores :bots :turn-time-limit))

(in-package :n-player-game)

(define-json-model bot-definition (command name))

(defmethod start-bot ((bot-definition bot-definition))
  (let ((command-parts (split "\\s+" (command bot-definition))))
    (make-instance 'concrete-bot :bot-process (run-bot (car command-parts) (cdr command-parts)))))

(defclass game-state ()
  ((bots :accessor bots :initarg :bots :initform nil)
   (turn-time-limit :accessor turn-time-limit :initarg :turn-time-limit :initform 1)))

(defgeneric is-finished? (game-state))
(defgeneric next-turn (game-state))
(defgeneric terminate-bots (game-state))
(defgeneric bot-scores (game-state))
(defgeneric get-bot-input (game-state))
(defgeneric update-game-state (game-state bot-output))
(defgeneric update-game-turn (game-state))

(defmethod n-player-game ((game-start game-state))
  (loop for game = game-start then
       (loop for current-game = game then (update-game-state current-game bot-output)
           for bot in (bots game)
           for bot-output = (when (not (equal (bot-status bot) :exited))
                              (bot-turn (get-bot-input game) bot (turn-time-limit game)))
           finally (return (update-game-turn game)))
     while (not (is-finished? game))
     finally (return (progn (terminate-bots game)
                            (bot-scores game)))))
