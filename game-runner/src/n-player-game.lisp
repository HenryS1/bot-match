(defpackage n-player-game
  (:use :cl :runtime :herodotus :cl-ppcre :alexandria)
  (:export :game-state :bot-definition :n-player-game :is-finished?
           :bot-status :get-bot-input :update-game-state :update-game-turn
           :tick :concrete-game
           :terminate-bots :bot-scores :bots :turn-time-limit))

(in-package :n-player-game)

(defclass game ())
(defgeneric is-finished? (game))
(defgeneric game-result (game))
(defgeneric get-players-input-for-turn (game))
(defgeneric advance-turn (player-moves game))
(defgeneric turn-time-limit (game))

(defmethod tick (bots (game game))
  (let* ((player-input-for-turn (get-players-input-for-turn game))
         (player-moves (mapcar (lambda (pi) 
                                 (let ((bot (gethash (car pi) bots)))
                                   (if bot 
                                       (cons (car pi) (bot-turn bot (cdr pi) (turn-time-limit game)))
                                       (cons (car pi ""))))) player-input-for-turn)))
    (advance-turn player-moves game))) 

(defparameter *termination-timeout* 0.5)

(defun terminate-bots (bots)
  (mapc #'interrupt-bot bots)
  (sleep *termination-timeout*)
  (mapc (lambda (bot) (when (not (equal (bot-status bot) :exited)) (kill-bot bot)))))

(defmethod n-player-game (bots (game game) turn-time-limit)
  (unwind-protect 
       (loop for gm first game then (tick bots gm turn-time-limit)
          until (is-finished? gm)
          finally (return gm))
    (terminate-bots bots)))
