(defpackage n-player-game
  (:use :cl :runtime :herodotus :cl-ppcre :alexandria)
  (:export :game :n-player-game :is-finished?
           :get-players-input-for-turn :advance-turn
           :tick
           :terminate-bots
           :game-result
           :turn-time-limit))

(in-package :n-player-game)

(defgeneric is-finished? (game))
(defgeneric game-result (game))
(defgeneric get-players-input-for-turn (game))
(defgeneric advance-turn (player-moves game))
(defgeneric turn-time-limit (game))

(defmethod tick (bots game)
  (let* ((player-input-for-turn (get-players-input-for-turn game))
         (player-moves (mapcar (lambda (pin) 
                                 (let ((bot (gethash (car pin) bots)))
                                   (if bot 
                                       (cons (car pin) (bot-turn bot
                                                                 (cdr pin) 
                                                                 (turn-time-limit game)))
                                       (cons (car pin) "")))) player-input-for-turn)))
    (advance-turn player-moves game)))

(defparameter *termination-timeout* 0.5)

(defun terminate-bots (bots)
  (let ((bs (hash-table-values bots)))
    (mapc #'interrupt-bot bs)
    (sleep *termination-timeout*)
    (mapc (lambda (bot) (when (not (equal (bot-status bot) :exited)) (kill-bot bot))) bs)))

(defmethod n-player-game (bots game turn-time-limit)
  (unwind-protect 
       (loop for gm = game then (tick bots gm)
          until (is-finished? gm)
          finally (return gm))
    (terminate-bots bots)))
