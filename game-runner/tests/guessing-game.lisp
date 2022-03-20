(defpackage :guessing-game
  (:use :cl 
        :runtime
        :cl-ppcre
        :n-player-game
        :alexandria
        :rove
        :trivia
        :monad
        :either
        :docker-client))

(in-package :guessing-game)

(defclass guessing-game ()
  ((numbers :accessor numbers :initarg :numbers :initform (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20))
   (player-guesses :accessor player-guesses :initarg :player-guesses :initform nil)
   (current-player-id :accessor current-player-id :initarg :current-player-id :initform "player1")
   (other-player-id :accessor other-player-id :initarg :other-player-id :initform "player2")
   (player-scores :accessor player-scores :initarg :player-scores :initform nil)))

(defmethod game-state ((game guessing-game))
  (format nil "Player scores ~a, Player guesses ~a" (player-scores game) (player-guesses game)))

(defmethod is-finished? ((game guessing-game))
  (null (numbers game)))

(defmethod game-result ((game guessing-game))
  (player-scores game))

(defmethod get-players-input-for-turn ((game guessing-game))
  (list (cons (current-player-id game) (format nil "~a~%" (car (numbers game))))))

(defmethod turn-time-limit ((game guessing-game)) 1)

(defun add-guess (old-guesses player-move)
  (let ((n (parse-integer (car (cdr player-move))))
        (guesses-for-player (cdr (assoc (car player-move) old-guesses :test 'equal))))
    (cons (cons (car player-move) (cons n guesses-for-player)) 
          (remove (car player-move) old-guesses :key #'car :test 'equal))))

(defun update-score (game)
  (lambda (old-scores player-move)
    (let* ((n (parse-integer (car (cdr player-move))))
           (target (+ (car (numbers game)) 1))
           (old-score (or (cdr (assoc (car player-move) old-scores :test 'equal)) 0))
           (new-score (+ old-score (- 1 (abs (- n target))))))
      (cons (cons (car player-move) new-score) 
            (remove (car player-move) old-scores :key #'car :test 'equal)))))

(defmethod input-parser ((game guessing-game)) #'read-output)

(defmethod advance-turn (player-moves game disqualified-players)
  (declare (ignore disqualified-players))
  (make-game-turn-result 
   :game (make-instance 'guessing-game 
                        :numbers (cdr (numbers game))
                        :player-guesses (reduce #'add-guess
                                                player-moves :initial-value (player-guesses game))
                        :current-player-id (other-player-id game)
                        :other-player-id (current-player-id game)
                        :player-scores (reduce (update-score game) player-moves 
                                               :initial-value (player-scores game)))
   :move-log (mapcar (lambda (m) (format nil "Player ~a, Move ~a" (car m) (cdr m))) 
                     player-moves)))

(defparameter *base-path* (directory-namestring #.*compile-file-truename*))

(defun create-bot (bot-file bot-directory)
  (let* ((config (make-docker-config 
                  "bot-match/lisp-base"
                  :command (split "\\s+" (format nil "ros +Q -- /bots/~a" bot-file))
                  :entrypoint (list "")
                  :open-stdin t
                  :volumes (list "/bots")
                  :readonly-rootfs t
                  :binds (list (format nil "~a:/bots" 
                                       (merge-pathnames bot-directory *base-path*))))))
    (create-container bot-file :docker-config config)))

(setup (create-bot "guessing-game-bot1.lisp" "bot1")
       (create-bot "guessing-game-bot2.lisp" "bot2"))

(defun cleanup-bot (bot-file)
  (stop-container bot-file)
  (remove-container bot-file))

(teardown 
  (handler-case (progn (cleanup-bot "guessing-game-bot1.lisp")
                       (cleanup-bot "guessing-game-bot2.lisp"))
    (error (e) (format t "Error during tear down ~a~%" e))))

(defun run-bots (logging-config)
  (loop for bot-base-path in (mapcar (lambda (dir) (merge-pathnames dir *base-path* ))
                                     '("bot1/" "bot2/"))
     for definition = (read-bot-definition (merge-pathnames "definition.json" bot-base-path))
     collect (start-bot-from-definition (name definition) 
                                        (logging-config-turns logging-config)
                                        *error-output*)))

(defun rights (results)
  (mapcar #'right-value (remove-if-not (lambda (r) 
                                         (match r ((type right) t)
                                                (otherwise nil))) results)))

(defun lefts (results)
  (mapcar #'left-err (remove-if-not (lambda (r) 
                                      (match r ((type left) t) 
                                             (otherwise nil))) results)))

(defmethod game-visualisation ((game guessing-game)) "Guessing game")

(defun run-guessing-game ()
  (format t "running guessing game~%")
  (let* ((*bot-initialisation-time* 10)
         (logging-config (make-logging-config :turns nil
                                              :moves nil
                                              :states nil
                                              :visualisation nil))
         (bot-initialisation (run-bots logging-config))
         (errors (lefts bot-initialisation))
         (started-bots (rights bot-initialisation)))
    (if (not (null errors))
        (fail (format nil "~{~a~}" errors))
        (let ((player-bots (alist-hash-table (pairlis '("player1" "player2") started-bots) :test 'equal))
              (game (make-instance 'guessing-game)))
          (n-player-game player-bots game logging-config)))))

(deftest guessing-game 
  (testing "should produce scores for bots"
    (let ((end-state (run-guessing-game)))
      (ok (equal (assoc "player1" (player-scores end-state) :test 'equal)
                 (cons "player1" 0)))
      (ok (equal (assoc "player2" (player-scores end-state) :test 'equal)
                 (cons "player2" 10))))))
