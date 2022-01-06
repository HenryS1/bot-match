(defpackage footsoldiers
  (:use :cl :herodotus :bind
        :alexandria :try :arrow-macros
        :iterate :trivia
        :metabang-bind
        :monad :maybe
        :either))

(in-package :footsoldiers)

(defstruct soldier 
  (soldier-pos soldier-pos)
  (soldier-health soldier-health)
  (soldier-type soldier-type)
  (soldier-team soldier-team))

(defstruct base 
  (base-team base-team))

(defstruct game 
  (game-map game-map)
  (game-turns-remaining game-turns-remaining)
  (game-player1 game-player1)
  (game-player2 game-player2))

(defstruct player 
  (player-team player-team)
  (player-money player-money)
  (player-base player-base)
  (player-health player-health))

(defstruct build
  (build-soldier-type build-soldier-type)
  (build-start build-start)
  (build-destination build-destination))

(defconstant soldier-types '(:scout :tank :assassin))
(defconstant team '(:one :two))

(defun speed (soldier-type)
  (case soldier-type
    (:scout 2)
    (:assassin 5)
    (:tank 2)))

(defun armour (soldier-type)
  (case soldier-type
    (:scout 2)
    (:assassin 1)
    (:tank 4)))

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

(defun max-distance-from-base () 5)

(defun sqr (x) (* x x))

(defun x (pos) (car pos))
(defun y (pos) (cdr pos))

(defmethod distance (p1 p2) 
  (+ (+ (abs (- (x p1) (x p2))) (abs (- (y p1) (y p2))))))

(defun flip (f) (lambda (a b) (funcall f b a)))

(defun neighbours (pos mp)
  (->> (mdo (x (list (- (x pos) 1) (x pos) (+ (x pos) 1)))
            (y (list (- (y pos) 1) (y pos) (+ (y pos) 1)))
            (cons x y))
    (remove-if-not (lambda (p) (gethash p mp)))))

(defun close-enough (range p1 p2)
  (<= (distance p1 p2) range))

(defun reachable-positions (range p1 mp)
  (let ((seen (make-hash-table :test 'equal)))
    (labels ((rec (current)
               (let ((nbrs (remove-if-not (lambda (nbr) (and (not (gethash nbr seen))
                                                             (close-enough range nbr p1)))
                                          (neighbours current mp))))
                 (mapc (lambda (nbr) (setf (gethash nbr seen) t)) nbrs)
                 (mapc #'rec nbrs))))
      (rec p1))))

(defmethod closest-reachable-position ((s soldier) mp)
  (let ((range (speed (soldier-type soldier)))
        (origin (soldier-pos s))
        (dest (soldier-destination s)))
    (iter (for (p . _) in-hashtable (reachable-positions range origin mp))
          (reducing p by (lambda (a b) (if (< (distance a dest) (distance b dest) a b))) 
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

(defun move-soldiers (gm)
  (let ((es (sort (hash-table-alist (game-map gm)) #'pair-less :key #'car)))
    (iter (for (p . e) in es)
          (when (is-soldier e)
            (move-soldier mp e)))))

(defmethod eligible-target ((s soldier) e) 
  (match e
    ((soldier _ _ _ soldier-team) 
     (/= soldier-team (soldier-team s)))
    ((base base-team)
     (/= base-team (base-team e)))))

(defmethod find-target ((s soldier) mp)
  (bind (((x . y) (soldier-pos s))
         (search-candidates (list (cons (- x 1) (- y 1))
                                  (cons (- x 1) y)
                                  (cons (- x 1) (+ y 1))
                                  (cons x (+ y 1))
                                  (cons (+ x 1) (+ y 1))
                                  (cons (+ x 1) y)
                                  (cons (+ x 1) (- y 1))
                                  (cons x (- y 1)))))
    (find (lambda (p) 
            (and (gethash p mp)
                 (eligible-target s (gethash p mp)))) search-candidates)))

(defmethod attack-soldier ((s1 soldier) (s2 soldier) mp)
  (let ((new-health (- (soldier-health s2) 
                       (max 0 (- (damage (soldier-type s1)) (armour (soldier-type s2)))))))
    (if (= new-health 0)
        (remhash (soldier-pos s2) mp)
        (setf (gethash (soldier-pos s2) new-mp)
              (let ((new-soldier (copy-structure s2)))
                (setf (soldier-health new-soldier) new-health)
                new-soldier)))))

(defmethod attack-base ((s soldier) (b base) (gm game))
  (let ((new-p2 (copy-structure (player2 gm))))
    (setf (player-health new-p2
                     (max 0 (- (player-health new-p2) (damage (soldier-type s))))))
    (setf (game-player2 gm) new-p2)))

(defmethod attack-target ((s soldier) e (gm game))
  (match e
    ((type 'soldier) (attack-soldier s e (game-map gm)))
    ((type 'base) (attack-base s e gm))))

(defmethod make-soldier-attack ((gm game) (s soldier))
  (let ((target (find-target s (game-map gm))))
    (when target
      (attack-target s target gm))))

(defmethod make-soldiers-attack ((gm game))
  (let ((entries (sort (hash-table-alist (game-map gm)) #'pair-less :key #'car)))
    (iter (for (p . e) in entries)
          (match e
            ((type 'soldier) (make-soldier-attack gm e))))))

(defmethod close-enough-to-base (pos (player player))
  (<= (distance p (player-base player)) max-distance-from-base))

(defmethod build-soldier (soldier-type start destination (gm game) (new-gm game))
  (if (< (player-money (game-player1 gm)) (cost soldier-type))
      (left "Not enough money")
      (if (not (close-enough-to-base start (game-player1 gm)))
          (left "Too far from base")
          (if (gethash start (game-map gm))
              (left "Position is occupied")
              (let* ((s (make-soldier :soldier-pos start
                                      :soldier-health (initial-health soldier-type)
                                      :soldier-type soldier-type
                                      :soldier-destination destination
                                      :soldier-team (player-team (game-player1 gm))))
                     (new-player1 (copy-structure (game-player1 gm))))
                (setf (player-money new-player1) (- (player-money new-player1) (cost soldier-type)))
                (setf (gethash start (game-map new-gm)) s)
                (setf (game-player1 new-gm) new-player1)
                (right new-gm))))))

(defmethod apply-move (move (gm game) (new-gm game))
  (match move
    ((build tp start destination) (build-soldier soldier-type start destination gm new-gm))
    (:no-op (right new-gm))))

(defmethod game-over ((game game))
  (or (= (game-turns-remaining game) 0) 
      (= (player-health (game-player1 game)) 0)
      (= (player-health (game-player2 game)) 0)))

(defmethod determine-result ((game game))
  (if (= (player-health (game-player1 game)) 0) 
      (game-player2 game)
      (if (= (player-health (game-player2 game)) 0)
          (game-player1 game)
          nil)))

(defmethod tick (move (gm game))
  (let ((new-mp (copy-hash-table (game-map gm)))
        (new-gm (copy-structure gm)))
    (setf (game-map new-gm) new-mp)
    (match (apply-move move gm new-gm)
      ((left e) 
       (move-soldiers new-gm)
       (make-soldiers-attack new-gm))
      ((right updated)
       (move-soldiers updated)
       (make-soldiers-attack updated)
       (if (game-over updated)
           (determine-result game-after-moves)
           (let ((ticked-game (copy-structure game)))
             (setf (game-turns-remaining ticked-game) (- (game-turns-remaining ticked-game) 1))
             (rotatef (game-player1 ticked-game) (game-player2 ticked-game))))))))

