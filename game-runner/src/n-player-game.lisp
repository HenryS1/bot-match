(defpackage n-player-game
  (:use :cl :runtime :herodotus :cl-ppcre :alexandria)
  (:export :game :n-player-game :is-finished?
           :get-players-input-for-turn :advance-turn
           :tick
           :terminate-bots
           :game-result
           :turn-time-limit
           :input-parser
           :make-game-turn-result
           :make-logging-config
           :game-state
           :game-turn-result-game))

(in-package :n-player-game)

(defstruct game-turn-result game move-log)
(defstruct logging-config turns moves states)

(defgeneric is-finished? (game))
(defgeneric game-result (game))
(defgeneric game-state (game))
(defgeneric get-players-input-for-turn (game))
(defgeneric advance-turn (player-moves game))
(defgeneric input-parser (game))
(defgeneric turn-time-limit (game))

(defmethod tick (bots game logging-config)
  (let* ((player-input-for-turn (get-players-input-for-turn game))
         (turn-results (mapcar (lambda (pin) 
                                 (let ((bot (gethash (car pin) bots)))
                                   (if bot 
                                       (cons (car pin)
                                             (bot-turn bot
                                                       (cdr pin) 
                                                       (turn-time-limit game)
                                                       (input-parser game)))
                                       (cons (car pin) 
                                             (make-bot-turn-result 
                                              :output ""
                                              :logs (list (format nil "Failed to find bot for player id ~a" (car pin)))))))) 
                               player-input-for-turn)))
    (mapc (lambda (res) (mapc (lambda (l) (format (logging-config-turns logging-config) "~a~%" l)) 
                              (bot-turn-result-logs (cdr res)))) turn-results)
    (let ((turn-result (advance-turn (mapcar (lambda (res) (cons (car res) (bot-turn-result-output (cdr res)))) 
                                 turn-results) game)))
      (mapc (lambda (l) (format (logging-config-moves logging-config) "~a~%" l)) 
            (game-turn-result-move-log turn-result))
      (format (logging-config-states logging-config) "~a~%" 
              (game-state (game-turn-result-game turn-result)))
      (game-turn-result-game turn-result))))

(defparameter *termination-timeout* 0.5)

(defun terminate-bots (bots)
  (let ((bs (hash-table-values bots)))
    (mapc #'interrupt-bot bs)
    (sleep *termination-timeout*)
    (mapc (lambda (bot) (when (not (equal (bot-status bot) :exited)) (kill-bot bot))) bs)))

(defmethod n-player-game (bots game logging-config)
  (unwind-protect 
       (loop for gm = game then (tick bots gm logging-config)
          until (is-finished? gm)
          finally (return gm))
    (terminate-bots bots)))
