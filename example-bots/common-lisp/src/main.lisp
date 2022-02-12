(defpackage bot-match-bot
  (:use :cl :herodotus)
  (:export :main))
(in-package :bot-match-bot)

(define-json-model coord (x y))
(define-json-model player (team money (base coord) health))
(define-json-model input (you (game-map () "map") turns-remaining
                              (player1 player) (player2 player)))

(defun read-input ()
  (loop for input = (input-json:from-json *standard-input*)
     while (listen)
     finally (return (when input
                       (cons (if (string= (you input) "player1")
                                 (player1 input)
                                 (player2 input))
                             (if (string= (you input) "player1")
                                 (player2 input)
                                 (player1 input)))))))

(defun run ()
  (format t "READY~%")
  (loop for (my-player . other-player) = (read-input)
     if (and my-player other-player (>= (money my-player) 10))
     do (let ((my-base-coord (base my-player))
              (enemy-base-coord (base other-player)))
          (format t "~a~%" (format nil "BUILD SCOUT (~a, ~a) (~a, ~a)"
                                   (x my-base-coord)
                                   (+ (y my-base-coord) 1)
                                   (x enemy-base-coord)
                                   (y enemy-base-coord))))
     else do (format t "NO-OP~%")))

(defun main (&rest argv)
  "Start the bot."
  (declare (ignorable argv))
  (run))
