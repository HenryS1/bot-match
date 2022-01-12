(defpackage footsoldiers
  (:use :cl :herodotus :bind
        :alexandria :try
        :iterate :trivia
        :trivia.ppcre
        :metabang-bind
        :monad 
        :n-player-game
        :anaphora
        :either)
  (:export :distance :neighbours
           :make-soldier :reachable-positions
           :soldier-pos :closest-reachable-position
           :soldier-destination
           :move-soldier
           :move-soldiers
           :eligible-target
           :make-base
           :find-target
           :soldier-health
           :attack-soldier
           :attack-base
           :player-health
           :make-game
           :make-player
           :attack-target
           :make-soldier-attack
           :make-soldiers-attack
           :game-player1
           :game-player2
           :game-map
           :close-enough-to-base
           :build-soldier
           :apply-move
           :make-build))

(in-package :footsoldiers)

(defstruct soldier pos health type team destination)

(defun soldier-alist (soldier)
  (list
   (cons "pos" (soldier-pos soldier))
   (cons "health" (soldier-health soldier))
   (cons "team" (soldier-team soldier))
   (cons "destination" (soldier-destination soldier))))

(defstruct base team)

(defun base-alist (base)
  (list 
   (cons "team" (base-team base))))

(defstruct game map turns-remaining player1 player2)

(defun map-to-alist (mp)
  (mapcar (lambda (e) 
            (match (cdr e)
              ((type soldier)
               (cons (car e) (soldier-alist (cdr e))))
              ((type base)
               (cons (car e) (base-alist (cdr e))))))
          (hash-table-alist mp)))

(defun game-alist (game)
  (list
   (cons "map" (map-to-alist (game-map game)))
   (cons "turns-remaining" (game-turns-remaining game))
   (cons "player1" (player-alist (game-player1 game)))
   (cons "player2" (player-alist (game-player2 game)))))

(defstruct player team money base health)

(defun player-alist (player)
  (list 
   (cons "team" (player-team player))
   (cons "money" (player-money player))
   (cons "base" (player-base player))
   (cons "health" (player-health player))))

(defstruct build soldier-type start destination)

(defun build-alist (build)
  (list 
   (cons "soldier-type" (build-soldier-type build))
   (cons "start" (build-start build))
   (cons "destination" (build-destination build))))

(defstruct move-result errors updated-game)

(defparameter *soldier-types* '(:scout :tank :assassin))
(defparameter *team* '(:one :two))

(defun initial-health (soldier-type) (declare (ignore soldier-type)) 6)

(defun soldier-speed (soldier-type)
  (case soldier-type
    (:scout 3)
    (:assassin 5)
    (:tank 2)))

(defun damage (soldier-type)
  (case soldier-type
    (:scout 2)
    (:assassin 5)
    (:tank 2)))

(defun cost (soldier-type)
  (case soldier-type
    (:scout 10)
    (:tank 12)
    (:assassin 14)))

(defun money-per-turn () 3)

(defun max-distance-from-base () 5)

(defun sqr (x) (* x x))

(defun x (pos) (car pos))
(defun y (pos) (cdr pos))

(defmethod distance (p1 p2) 
  (+ (+ (abs (- (x p1) (x p2))) (abs (- (y p1) (y p2))))))

(defun neighbours (pos mp)
  (let ((ns (list (cons (x pos) (+ (y pos) 1))
                  (cons (- (x pos) 1) (y pos))
                  (cons (x pos) (- (y pos) 1))
                  (cons (+ (x pos) 1) (y pos)))))
    (remove-if (lambda (p) (gethash p mp)) ns)))

(defun close-enough (range p1 p2)
  (<= (distance p1 p2) range))

(defun reachable-positions (range p1 mp)
  (let ((seen (make-hash-table :test 'equal)))
    (setf (gethash p1 seen) t)
    (labels ((rec (current)
               (let ((nbrs (remove-if-not (lambda (nbr) (and (not (gethash nbr seen))
                                                             (close-enough range nbr p1)))
                                          (neighbours current mp))))
                 (mapc (lambda (nbr) (setf (gethash nbr seen) t)) nbrs)
                 (mapc #'rec nbrs))))
      (rec p1)
      seen)))

(defmethod closest-reachable-position ((s soldier) mp)
  (let ((range (soldier-speed (soldier-type s)))
        (origin (soldier-pos s))
        (dest (soldier-destination s)))
    (iter (for p in (sort (hash-table-keys (reachable-positions range origin mp)) #'pair-less))
          (reducing p by (lambda (a b) (if (< (distance b dest) (distance a dest)) b a)) 
                    initial-value origin))))

(defmethod move-soldier (mp (s soldier))
  (when (not (equal (soldier-pos s) (soldier-destination s)))
    (let* ((destination (closest-reachable-position s mp))
           (new-soldier (copy-structure s)))
      (setf (soldier-pos new-soldier) destination)
      (remhash (soldier-pos s) mp)
      (setf (gethash destination mp) new-soldier))))

(defmethod is-soldier ((s soldier)) t)
(defmethod is-soldier ((non-soldier t)) nil)

(defun pair-less (p1 p2)
  (or (< (car p1) (car p2))
      (and (= (car p1) (car p2))
           (< (cdr p1) (cdr p2)))))

(defun move-soldiers (mp)
  (let ((es (sort (hash-table-alist mp) #'pair-less :key #'car)))
    (iter (for (p . e) in es)
          (when (is-soldier e)
            (move-soldier mp e)))))

(defmethod eligible-target ((s soldier) e) 
  (match e
    ((soldier team) 
     (not (equalp team (soldier-team s))))
    ((base team)
     (not (equalp team (soldier-team s))))))

(defmethod find-target ((s soldier) mp)
  (bind (((x . y) (soldier-pos s))
         (search-candidates (list (cons x (+ y 1))
                                  (cons (- x 1) y)
                                  (cons x (- y 1))
                                  (cons (+ x 1) y))))
    (awhen (find-if (lambda (p) 
                (and (gethash p mp)
                     (eligible-target s (gethash p mp)))) search-candidates)
      (gethash it mp))))

(defmethod attack-soldier ((s1 soldier) (s2 soldier) mp)
  (let ((new-health (max 0 (- (soldier-health s2) (damage (soldier-type s1))))))
    (if (= new-health 0)
        (remhash (soldier-pos s2) mp)
        (setf (gethash (soldier-pos s2) mp)
              (let ((new-soldier (copy-structure s2)))
                (setf (soldier-health new-soldier) new-health)
                new-soldier)))))

(defmethod attack-base ((s soldier) (b base) (gm game))
  (let ((new-player (if (equalp (base-team b) (player-team (game-player1 gm)))
                        (copy-structure (game-player1 gm))
                        (copy-structure (game-player2 gm)))))
    (setf (player-health new-player) (max 0 (- (player-health new-player)
                                               (damage (soldier-type s)))))
    (if (equalp (base-team b) (player-team (game-player1 gm)))
        (setf (game-player1 gm) new-player)
        (setf (game-player2 gm) new-player))))

(defmethod attack-target ((s soldier) e (gm game))
  (match e
    ((type soldier) (attack-soldier s e (game-map gm)))
    ((type base) (attack-base s e gm))))

(defmethod make-soldier-attack ((gm game) (s soldier))
  (let ((target (find-target s (game-map gm))))
    (when target
      (attack-target s target gm))))

(defmethod make-soldiers-attack ((gm game))
  (let ((entries (sort (hash-table-alist (game-map gm)) #'pair-less :key #'car)))
    (iter (for (p . e) in entries)
          (match e
            ((type soldier) (make-soldier-attack gm e))))))

(defmethod close-enough-to-base (pos (player player))
  (<= (distance pos (player-base player)) (max-distance-from-base)))

(defmethod build-soldier (team soldier-type start destination (new-gm game))
  (let ((player (copy-structure (if (equalp team (player-team (game-player1 new-gm)))
                                    (game-player1 new-gm)
                                    (game-player2 new-gm)))))
    (if (< (player-money player) (cost soldier-type))
        (left "Not enough money")
        (if (not (close-enough-to-base start player))
            (left "Too far from base")
            (if (gethash start (game-map new-gm))
                (left "Position is occupied")
                (let* ((s (make-soldier :pos start
                                        :health (initial-health soldier-type)
                                        :type soldier-type
                                        :destination destination
                                        :team (player-team player))))
                  (setf (player-money player) (- (player-money player) (cost soldier-type)))
                  (setf (gethash start (game-map new-gm)) s)
                  (if (equalp team (player-team (game-player1 new-gm)))
                      (setf (game-player1 new-gm) player)
                      (setf (game-player2 new-gm) player))
                  (right new-gm)))))))

(defmethod apply-move (team move (new-gm game))
  (match move
    ((type build) 
     (build-soldier team 
                    (build-soldier-type move) 
                    (build-start move)
                    (build-destination move) new-gm))
    (:no-op (right new-gm))))

(defun combine-results (one other)
  (match (cons one other)
         ((cons (type move-result) (type move-result))
          (make-move-result :errors (append (move-result-errors one) (move-result-errors other)) 
                             :updated-game (move-result-updated-game other)))
         ((cons (type move-result) (type right))
          (make-move-result :errors (move-result-errors one) :updated-game (right-value other)))
         ((cons (type right) (type move-result))
          (make-move-result :errors (move-result-errors other) :updated-game (move-result-updated-game other)))
         ((cons (type right) (type right))
          (make-move-result :errors nil :updated-game (right-value other)))))

(defmethod apply-moves (moves (new-gm game))
  (if (null moves)
      (right new-gm)
      (bind (((team . move) (car moves)))
        (match (apply-move team move new-gm)
           ((left left-err) 
            (combine-results (make-move-result :errors (list left-err) :updated-game new-gm)
                             (apply-moves (cdr moves) new-gm)))
           ((right right-value) 
            (combine-results (right right-value)
                             (apply-moves (cdr moves) new-gm)))))))

(defmethod game-over ((game game))
  (or (= (game-turns-remaining game) 0) 
      (= (player-health (game-player1 game)) 0)
      (= (player-health (game-player2 game)) 0)))

(defmethod determine-result ((game game))
  (if (and (= (player-health (game-player1 game)) 0)
           (> (player-health (game-player2 game)) 0)) 
      (cons :win (game-player2 game))
      (if (and (= (player-health (game-player2 game)) 0)
               (> (player-health (game-player1 game)) 0))
          (cons :win (game-player1 game))
          :draw)))

(defmethod step-game (moves (gm game))
  (let ((new-mp (copy-hash-table (game-map gm)))
        (new-gm (copy-structure gm)))
    (setf (game-map new-gm) new-mp)
    (bind (((:structure move-result- errors updated-game) (apply-moves moves gm))
           (after-attack (make-soldiers-attack updated-game)))
      (let* ((ticked-game (copy-structure after-attack))
             (player1 (copy-structure (game-player1 ticked-game)))
             (player2 (copy-structure (game-player2 ticked-game))))
        (incf (player-money player1) (money-per-turn))
        (incf (player-money player2) (money-per-turn))
        (setf (game-turns-remaining ticked-game)
              (- (game-turns-remaining ticked-game) 1))
        (setf (game-player1 ticked-game) player2)
        (setf (game-player2 ticked-game) player1)
        (make-move-result :errors errors :updated-game ticked-game)))))

(defmethod is-finished? ((game game)) (game-over game))

(defmethod game-result ((game game)) (determine-result game))

(defmethod get-players-input-for-turn ((game game))
  (list (cons (player-team (game-player1 game)) 
              (format nil "~a" (game-alist game)))
        (cons (player-team (game-player2 game))
              (format nil "~a" (game-alist game)))))

(defmethod turn-time-limit ((game game)) 1)

(defmethod advance-turn (player-moves (game game))
  (bind (((:structure move-result- errors updated-game) (step-game player-moves game)))
    (when (not (null errors))
      (format t "Errors while applying game moves ~a~%" errors))
    updated-game))

(defun parse-move (player-move)
  (let ((mv (cadr player-move)))
    (match mv
      ((ppcre "BUILD (\\w+) \\((\\d+), (\\d+)\\) \\((\\d+), (\\d+)\\)" name 
              (read start-row) (read start-col) (read dest-row) (read dest-col))
       (make-build :soldier-type (intern (string-upcase name) "KEYWORD") 
                   :start (cons start-row start-col)
                   :destination (cons dest-row dest-col)))
      (t :no-op)))) 
