(defpackage footsoldiers/tests/footsoldiers
  (:use :cl
        :footsoldiers
        :alexandria
        :metabang-bind
        :monad
        :either
        :rove))

(in-package :footsoldiers/tests/footsoldiers)

(deftest distance
  (testing "determines the manhattan distance between two points"
    (ok (= (distance '(1 . 2) '(3 . 5)) 5))))

(defparameter *test-base1* (make-base :team "player1"))
(defparameter *test-base2* (make-base :team "player2"))

(defparameter *test-soldier1* (make-soldier :pos (cons 1 4) 
                                            :health 4 
                                            :type :scout
                                            :team "player1"
                                            :destination (cons 5 5)))

(defparameter *test-soldier2* (make-soldier :pos (cons 2 4)
                                            :health 3
                                            :type :assassin
                                            :team "player2"
                                            :destination (cons 3 1)))

(defparameter *test-soldier3* (make-soldier :pos (cons 4 4)
                                            :health 3
                                            :type :assassin
                                            :team "player1"
                                            :destination (cons 3 1)))

(defparameter *test-soldier4* (make-soldier :pos (cons 3 3)
                                            :health 3
                                            :type :assassin
                                            :team "player2"
                                            :destination (cons 3 1)))

(defparameter *test-soldier5* (make-soldier :pos (cons 3 5)
                                            :health 3
                                            :type :assassin
                                            :team "player2"
                                            :destination (cons 3 1)))

(deftest neighbours 
  (testing "finds all adjacent positions when none are occupied"
    (let ((mp (alist-hash-table (list (cons (cons 1 4) *test-soldier1*)) :test 'equal)))
      (ok (equal (neighbours (cons 1 4) mp)
                 (list (cons 1 5) (cons 0 4) (cons 1 3) (cons 2 4))))))
  (testing "doesn't find an adjacent position it is occupied"
    (let ((mp (alist-hash-table (list (cons (cons 1 4) *test-soldier1*) 
                                      (cons (cons 2 4) *test-soldier2*)) :test 'equal)))
      (ok (equal (neighbours (cons 1 4) mp)
                 (list (cons 1 5) (cons 0 4) (cons 1 3)))))))

(deftest reachable-positions
  (testing "finds all positions within range of soldier when none are obstructed"
    (let ((mp (alist-hash-table (list (cons (soldier-pos *test-soldier1*) *test-soldier1*)) 
                                :test 'equal)))
      (bind (((x . y) (soldier-pos *test-soldier1*)))
        (ok (equalp (reachable-positions 3 (soldier-pos *test-soldier1*) mp)
                    (alist-hash-table (mapcar (lambda (p) (cons p t))
                                              (list 
                                               (cons (- x 3) y)
                                               (cons (- x 2) y)
                                               (cons (- x 2) (- y 1))
                                               (cons (- x 2) (+ y 1))
                                               (cons (- x 1) y)
                                               (cons (- x 1) (- y 1))
                                               (cons (- x 1) (- y 2))
                                               (cons (- x 1) (+ y 1))
                                               (cons (- x 1) (+ y 2))
                                               (cons x y)
                                               (cons x (- y 1))
                                               (cons x (- y 2))
                                               (cons x (- y 3))
                                               (cons x (+ y 1))
                                               (cons x (+ y 2))
                                               (cons x (+ y 3))
                                               (cons (+ x 1) y)
                                               (cons (+ x 1) (- y 1))
                                               (cons (+ x 1) (- y 2))
                                               (cons (+ x 1) (+ y 1))
                                               (cons (+ x 1) (+ y 2))
                                               (cons (+ x 2) y)
                                               (cons (+ x 2) (- y 1))
                                               (cons (+ x 2) (+ y 1))
                                               (cons (+ x 3) y))) :test 'equal))))))
  (testing "doesn't find nearby positions that are blocked"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier1* *test-soldier2*
                                              *test-soldier3* *test-soldier4*
                                              *test-soldier5*)) :test 'equal)))
      (bind (((x . y) (soldier-pos *test-soldier1*)))
        (ok (equalp (reachable-positions 3 (soldier-pos *test-soldier1*) mp)
                    (alist-hash-table (mapcar (lambda (p) (cons p t))
                                              (list 
                                               (cons (- x 3) y)
                                               (cons (- x 2) y)
                                               (cons (- x 2) (- y 1))
                                               (cons (- x 2) (+ y 1))
                                               (cons (- x 1) y)
                                               (cons (- x 1) (- y 1))
                                               (cons (- x 1) (- y 2))
                                               (cons (- x 1) (+ y 1))
                                               (cons (- x 1) (+ y 2))
                                               (cons x y)
                                               (cons x (- y 1))
                                               (cons x (- y 2))
                                               (cons x (- y 3))
                                               (cons x (+ y 1))
                                               (cons x (+ y 2))
                                               (cons x (+ y 3))
                                               (cons (+ x 1) (- y 1))
                                               (cons (+ x 1) (- y 2))
                                               (cons (+ x 1) (+ y 1))
                                               (cons (+ x 1) (+ y 2)))) :test 'equal)))))))

(deftest closest-reachable-position 
  (testing "chooses the reachable position closest to the destination"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier1* *test-soldier2*
                                              *test-soldier3* *test-soldier4*
                                              *test-soldier5*))
                                 :test 'equal)))
      (ok (equal (closest-reachable-position *test-soldier1* mp)
                 (cons 2 5)))))
  (testing "chooses the least lexicographic position when multiple closest positions exist"
    (let ((new-soldier1 (copy-structure *test-soldier1*)))
      (setf (soldier-destination new-soldier1) (cons 2 1))
      (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                          (list new-soldier1 *test-soldier2*
                                                *test-soldier3* *test-soldier4*
                                                *test-soldier5*))
                                  :test 'equal)))
        (ok (equal (closest-reachable-position new-soldier1 mp)
                   (cons 1 1)))))))

(deftest move-soldier 
  (testing "moves a soldier to the reachable position closest to it's destination"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier1* *test-soldier2*
                                              *test-soldier3* *test-soldier4*
                                              *test-soldier5*))
                                 :test 'equal)))
      (move-soldier mp *test-soldier1*)
      (let ((new-soldier1 (copy-structure *test-soldier1*)))
        (setf (soldier-pos new-soldier1) (cons 2 5))
        (ok (equalp mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                                 (list new-soldier1 *test-soldier2*
                                                       *test-soldier3* *test-soldier4*
                                                       *test-soldier5*))
                                         :test 'equal)))))))

(deftest move-soldiers
  (testing "moves all soldiers to the the reachable position closest to their destinations"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier1* *test-soldier2*)) :test 'equal)))
      (let* ((new-soldier1 (copy-structure *test-soldier1*))
             (new-soldier2 (copy-structure *test-soldier2*)))
        (setf (soldier-pos new-soldier1) (cons 3 5))
        (setf (soldier-pos new-soldier2) (cons 3 1))
        (move-soldiers mp)
        (ok (equalp mp
                    (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                              (list new-soldier1 new-soldier2)) :test 'equal))))))

  (testing "moves soldiers in lexicographical order of their starting coordinate"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier2* *test-soldier3*)) :test 'equal)))
      (let* ((new-soldier2 (copy-structure *test-soldier2*))
             (new-soldier3 (copy-structure *test-soldier3*)))
        (setf (soldier-pos new-soldier2) (cons 3 1))
        (setf (soldier-pos new-soldier3) (cons 2 1))
        (move-soldiers mp)
        (ok (equalp mp
                    (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                              (list new-soldier2 new-soldier3)) :test 'equal)))))))

(deftest eligible-target 
  (testing "returns false for a soldier from the same team as the attacker"
    (ok (not (eligible-target *test-soldier2* *test-soldier4*))))
  (testing "returns true for a soldier from the opposing team"
    (ok (eligible-target *test-soldier1* *test-soldier2*)))
  (testing "returns false for the base from a soldier's team"
    (ok (not (eligible-target *test-soldier1* *test-base1*))))
  (testing "returns true for the base from an enemy soldier's team"
    (ok (eligible-target *test-soldier1* *test-base2*))))

(deftest find-target
  (testing "finds the first adjacent target clockwise starting from the bottom"
    (let ((new-soldier4 (copy-structure *test-soldier4*))
          (new-soldier3 (copy-structure *test-soldier3*)))
      (setf (soldier-pos new-soldier3) (cons 1 5))
      (setf (soldier-pos new-soldier4) (cons 0 4))
      (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                          (list *test-soldier1* 
                                                *test-soldier2*
                                                new-soldier3
                                                new-soldier4)) :test 'equal)))
        (ok (equalp (find-target *test-soldier1* mp) new-soldier4))))))

(deftest attack-soldier 
  (testing "decreases the health of a soldier by the difference between armour and damage"
    (let ((new-soldier2 (copy-structure *test-soldier2*))
          (mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier1* *test-soldier2*)) :test 'equal)))
      (setf (soldier-health new-soldier2) 1)
      (attack-soldier *test-soldier1* *test-soldier2* mp)
      (ok (equalp mp
                  (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                            (list *test-soldier1* new-soldier2)) :test 'equal)))))
  (testing "removes a soldier from the map when it's health reaches zero"
    (let ((new-soldier2 (copy-structure *test-soldier2*)))
      (setf (soldier-health new-soldier2) 2)
      (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                          (list *test-soldier1* new-soldier2)) :test 'equal)))
        (attack-soldier *test-soldier1* new-soldier2 mp)
        (ok (equalp mp
                    (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                              (list *test-soldier1*)) :test 'equal))))))) 

(deftest attack-base
  (testing "decreases the attacked player's health"
    (let* ((mp (alist-hash-table (list (cons (soldier-pos *test-soldier1*) *test-soldier1*)
                                       (cons (cons 0 4) *test-base2*)
                                       (cons (cons 10 13) *test-base1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 10 :base (cons 10 13) :health 20))
           (player2 (make-player :team "player2" :money 7 :base (cons 0 4) :health 13))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (attack-base *test-soldier1* *test-base2* gm)
      (let ((new-player2 (copy-structure player2)))
        (setf (player-health new-player2) 11)
        (ok (equalp gm 
                    (make-game :map mp :turns-remaining 20 :player1 player1 :player2 new-player2))))))
  (testing "doesn't reduce the attacked player's health below zero"
    (let* ((mp (alist-hash-table (list (cons (soldier-pos *test-soldier1*) *test-soldier1*)
                                       (cons (cons 0 4) *test-base2*)
                                       (cons (cons 10 13) *test-base1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 10 :base (cons 10 13) :health 20))
           (player2 (make-player :team "player2" :money 7 :base (cons 0 4) :health 1))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (attack-base *test-soldier1* *test-base2* gm)
      (let ((new-player2 (copy-structure player2)))
        (setf (player-health new-player2) 0)
        (ok (equalp gm 
                    (make-game :map mp :turns-remaining 20 :player1 player1 :player2 new-player2)))))))

(deftest attack-target
  (testing "attacks a soldier when the target is a soldier"
    (let* ((mp (alist-hash-table (list (cons (soldier-pos *test-soldier1*) *test-soldier1*)
                                       (cons (soldier-pos *test-soldier2*) *test-soldier2*)
                                       (cons (cons 7 9) *test-base2*)
                                       (cons (cons 10 13) *test-base1*)) :test 'equal))
           (mp-copy (copy-hash-table mp))
           (player1 (make-player :team "player1" :money 10 :base (cons 10 13) :health 20))
           (player2 (make-player :team "player2" :money 7 :base (cons 0 4) :health 10))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (gm2 (make-game :map mp-copy :turns-remaining 20 :player1 player1 :player2 player2)))
      (attack-target *test-soldier1* *test-soldier2* gm)
      (attack-soldier *test-soldier1* *test-soldier2* mp-copy)
      (ok (equalp gm gm2))))
  (testing "attacks the base when target is a base"
    (let* ((mp (alist-hash-table (list (cons (soldier-pos *test-soldier1*) *test-soldier1*)
                                       (cons (cons 0 4) *test-base2*)
                                       (cons (cons 10 13) *test-base1*)) :test 'equal))
           (mp-copy (copy-hash-table mp))
           (player1 (make-player :team "player1" :money 10 :base (cons 10 13) :health 20))
           (player2 (make-player :team "player2" :money 7 :base (cons 0 4) :health 10))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (gm2 (make-game :map mp-copy :turns-remaining 20 :player1 player1 :player2 player2)))
      (attack-target *test-soldier1* *test-base2* gm)
      (attack-base *test-soldier1* *test-base2* gm2)
      (ok (equalp gm gm2)))))

(deftest make-soldier-attack
  (testing "makes a soldier find a target and attack it"
    (let* ((mp (alist-hash-table (list (cons (soldier-pos *test-soldier1*) *test-soldier1*)
                                       (cons (soldier-pos *test-soldier2*) *test-soldier2*)
                                       (cons (cons 7 9) *test-base2*)
                                       (cons (cons 10 13) *test-base1*)) :test 'equal))
           (mp-copy (copy-hash-table mp))
           (player1 (make-player :team "player1" :money 10 :base (cons 10 13) :health 20))
           (player2 (make-player :team "player2" :money 7 :base (cons 0 4) :health 10))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (gm2 (make-game :map mp-copy :turns-remaining 20 :player1 player1 :player2 player2)))
      (make-soldier-attack gm *test-soldier1*)
      (attack-target *test-soldier1* *test-soldier2* gm2)
      (ok (equalp gm gm2)))))

(deftest make-soldiers-attack
  (testing "makes all soldiers find a target and attack"
    (let* ((mp (alist-hash-table (list (cons (soldier-pos *test-soldier1*) *test-soldier1*)
                                       (cons (soldier-pos *test-soldier2*) *test-soldier2*)
                                       (cons (soldier-pos *test-soldier3*) *test-soldier3*)
                                       (cons (soldier-pos *test-soldier4*) *test-soldier4*)
                                       (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 10 :base (cons 4 3) :health 20))
           (player2 (make-player :team "player2" :money 7 :base (cons 5 4) :health 10))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (new-gm (make-soldiers-attack gm)))
      (ok (equal (player-health (game-player1 new-gm)) 15))
      (ok (equal (player-health (game-player2 new-gm)) 5))
      (let ((new-soldier2 (copy-structure *test-soldier2*)))
        (setf (soldier-health new-soldier2) 1)
        (ok (equalp (game-map new-gm)
                    (alist-hash-table (list 
                                       (cons (soldier-pos new-soldier2) new-soldier2)
                                       (cons (soldier-pos *test-soldier3*) *test-soldier3*)
                                       (cons (soldier-pos *test-soldier4*) *test-soldier4*)
                                       (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal)))))))

(deftest close-enough-to-base 
  (let ((player (make-player :team "player1" :money 10 :base (cons 4 3) :health 20)))
    (testing "is true when a position is closer than the max distance from a player's base"
      (ok (close-enough-to-base (cons 2 6) player)))
    (testing "is false when a position is further than the max distance from a player's base"
      (ok (not (close-enough-to-base (cons 2 7) player))))))

(deftest build-soldier
  (testing "fails when a soldier costs more than a player's money"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 9 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))) 
      (ok (equalp (build-soldier "player1" :scout (cons 4 4) (cons 8 10) gm)
                  (left "Not enough money")))))
  (testing "fails when the build position is already occupied"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 4 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (ok (equalp (build-soldier "player1" :scout (cons 4 4) (cons 8 10) gm)
                  (left "Position is occupied")))))
  (testing "fails when the build position is too far from the player's base"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (ok (equalp (build-soldier "player1" :scout (cons 6 7) (cons 8 10) gm)
                  (left "Too far from base")))))
  (testing "adds a soldier to the map and reduces the player's money"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (result (build-soldier "player1" :scout (cons 5 6) (cons 8 10) gm))
           (new-soldier (make-soldier :pos (cons 5 6) :health 6 
                                      :type :scout :team "player1" :destination (cons 8 10)))
           (new-player1 (make-player :team "player1" :money 10 :base (cons 4 3) :health 25))
           (new-mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                           (cons (cons 5 4) *test-base2*)
                                           (cons (cons 5 6) new-soldier)) :test 'equal))
           (new-gm (make-game :map new-mp :turns-remaining 20 :player1 new-player1 :player2 player2)))
      (ok (equalp result (right new-gm))))))

(deftest apply-move 
  (testing "does nothing for no-op"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 9 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))) 
      (ok (equalp (apply-move "player1" :no-op gm)
                  (right gm)))))
  (testing "builds a soldier when the command is build"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (result (apply-move "player1" (make-build :soldier-type :scout 
                                                     :start (cons 5 6)
                                                     :destination (cons 8 10)) gm))
           (new-soldier (make-soldier :pos (cons 5 6) :health 6 
                                      :type :scout :team "player1" :destination (cons 8 10)))
           (new-player1 (make-player :team "player1" :money 10 :base (cons 4 3) :health 25))
           (new-mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                           (cons (cons 5 4) *test-base2*)
                                           (cons (cons 5 6) new-soldier)) :test 'equal))
           (new-gm (make-game :map new-mp :turns-remaining 20 :player1 new-player1 :player2 player2)))
      (ok (equalp result (right new-gm))))))

(deftest apply-moves 
  (testing "collects all move failures"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 4 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (moves (list (cons "player1" (make-build :soldier-type :scout
                                                    :start (cons 4 4)
                                                    :destination (cons 8 10)))
                        (cons "player2" (make-build :soldier-type :assassin
                                                    :start (cons 3 3)
                                                    :destination (cons 9 6)))))
           (result (apply-moves moves gm)))
      (ok (equalp result
                  (make-move-result :errors (list "Position is occupied" "Not enough money")
                                    :updated-game gm)))))  
  (testing "applies moves for both players"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 15 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (moves (list (cons "player1" (make-build :soldier-type :scout
                                                    :start (cons 4 4)
                                                    :destination (cons 8 10)))
                        (cons "player2" (make-build :soldier-type :assassin
                                                    :start (cons 3 3)
                                                    :destination (cons 9 6)))))
           (new-mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                           (cons (cons 5 4) *test-base2*)
                                           (cons (cons 4 4) 
                                                 (make-soldier :pos (cons 4 4)
                                                               :health 6
                                                               :type :scout
                                                               :team "player1"
                                                               :destination (cons 8 10)))
                                           (cons (cons 3 3) 
                                                 (make-soldier :pos (cons 3 3)
                                                               :health 6
                                                               :type :assassin
                                                               :team "player2"
                                                               :destination (cons 9 6)))) 
                                     :test 'equal))
           (new-player1 (make-player :team "player1" :money 10 :base (cons 4 3) :health 25))
           (new-player2 (make-player :team "player2" :money 1 :base (cons 5 4) :health 24))
           (new-gm (make-game :map new-mp :turns-remaining 20 
                              :player1 new-player1
                              :player2 new-player2))
           (result (apply-moves moves gm)))
      (ok (equalp result
                  (make-move-result :errors nil :updated-game new-gm))))))

(deftest game-over
  (testing "is true when there are no turns remaining"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 1 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 0 :player1 player1 :player2 player2)))
      (ok (game-over gm))))
  (testing "is true when player1 has no health remaining"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 1 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 0))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2)))
      (ok (game-over gm))))
  (testing "is true when player2 has no health remaining"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 1 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 0))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2)))
      (ok (game-over gm))))
  (testing "is false otherwise"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 1 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2)))
      (ok (not (game-over gm)))))) 

(deftest determine-result 
  (testing "returns win for player1 when player2 has no health and player1 has non-zero health"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 0))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2)))
      (ok (equalp (determine-result gm) (cons :win "player1")))))
  (testing "returns win for player2 when player1 has no health and player2 has non-zero health"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 0))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2)))
      (ok (equalp (determine-result gm) (cons :win "player2")))))
  (testing "returns draw otherwise"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 0 :player1 player1 :player2 player2)))
      (ok (equalp (determine-result gm) :draw)))))

(deftest step-game
  (testing "applies moves, increments available money and reduces available turns"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 15 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (moves (list (cons "player1" (make-build :soldier-type :scout
                                                    :start (cons 6 5)
                                                    :destination (cons 5 5)))
                        (cons "player2" (make-build :soldier-type :assassin
                                                    :start (cons 3 2)
                                                    :destination (cons 3 3)))))
           (new-mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                           (cons (cons 5 4) *test-base2*)
                                           (cons (cons 5 5) 
                                                 (make-soldier :pos (cons 5 5)
                                                               :health 6
                                                               :type :scout
                                                               :team "player1"
                                                               :destination (cons 5 5)))
                                           (cons (cons 3 3) 
                                                 (make-soldier :pos (cons 3 3)
                                                               :health 6
                                                               :type :assassin
                                                               :team "player2"
                                                               :destination (cons 3 3)))) 
                                     :test 'equal))
           (new-player1 (make-player :team "player1" 
                                     :money (+ 10 (money-per-turn)) 
                                     :base (cons 4 3) :health 20))
           (new-player2 (make-player :team "player2" 
                                     :money (+ 1 (money-per-turn)) 
                                     :base (cons 5 4) :health 22))
           (new-gm (make-game :map new-mp :turns-remaining 19
                              :player1 new-player1
                              :player2 new-player2))
           (result (step-game moves gm)))
      (ok (equalp result (make-move-result :errors nil :updated-game new-gm))))))

(deftest get-players-input-for-turn
  (testing "provides the game as an alist to each of the players"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 1 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 7))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2))
           (map-repr (list (cons (cons 1 4) (list (cons "pos" (cons 1 4))
                                                  (cons "health" 4)
                                                  (cons "type" :scout)
                                                  (cons "team" "player1")
                                                  (cons "destination" (cons 5 5))))
                           (cons (cons 4 3) (list (cons "team" "player1")))
                           (cons (cons 5 4) (list (cons "team" "player2")))))
           (player1-repr (list (cons "team" "player1")
                               (cons "money" 20)
                               (cons "base" (cons 4 3))
                               (cons "health" 25)))
           (player2-repr (list (cons "team" "player2")
                               (cons "money" 3)
                               (cons "base" (cons 5 4))
                               (cons "health" 7)))
           (game-repr (list (cons "map" map-repr) 
                            (cons "turns-remaining" 10)
                            (cons "player1" player1-repr)
                            (cons "player2" player2-repr))))
      (ok (equalp (get-players-input-for-turn gm)
                  (list (cons "player1" (format nil "~a" game-repr))
                        (cons "player2" (format nil "~a" game-repr))))))))

