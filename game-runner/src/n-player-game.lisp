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
           :game-turn-result-game
           :logging-config-turns))

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
         (new-bots (make-hash-table :test 'equal))
         (turn-results (mapcar (lambda (pin) 
                                 (let ((bot (gethash (car pin) bots)))
                                   (if bot 
                                       (cons (car pin)
                                             (bot-turn bot
                                                       (cdr pin) 
                                                       (turn-time-limit game)
                                                       (logging-config-turns logging-config)
                                                       (input-parser game)))
                                       (cons (car pin) 
                                             (make-bot-turn-result 
                                              :updated-bot bot
                                              :output ""))))) 
                               player-input-for-turn)))
    (mapc (lambda (res) 
            (let ((updated-bot (bot-turn-result-updated-bot (cdr res))))
              (when updated-bot
                (setf (gethash (car res) new-bots) updated-bot))))
          turn-results)
    (maphash (lambda (p b) (when (not (gethash p new-bots)) (setf (gethash p new-bots) b))) bots)
    (let ((turn-result (advance-turn (mapcar (lambda (res) (cons (car res) (bot-turn-result-output (cdr res)))) 
                                 turn-results) game)))
      (mapc (lambda (l) (format (logging-config-moves logging-config) "~a~%" l)) 
            (game-turn-result-move-log turn-result))
      (format (logging-config-states logging-config) "~a~%" 
              (game-state (game-turn-result-game turn-result)))
      (cons new-bots (game-turn-result-game turn-result)))))

(defparameter *termination-timeout* 0.5)

(defun terminate-bots (bots)
  (let ((bs (hash-table-values bots)))
    (mapc #'interrupt-bot bs)
    (sleep *termination-timeout*)
    (mapc (lambda (bot) (when (not (equal (bot-status bot) :exited)) (kill-bot bot))) bs)))

(defmethod n-player-game (bots game logging-config)
  (unwind-protect 
       (loop for (cur-bots . gm) = (cons bots game) then (tick cur-bots gm logging-config)
          until (is-finished? gm)
          finally (return gm))
    (terminate-bots bots)))
