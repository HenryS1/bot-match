(defpackage footsoldiers/tests/footsoldiers
  (:use :cl
        :footsoldiers
        :alexandria
        :metabang-bind
        :n-player-game
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
                                            :destination (cons 5 5)
                                            :attack-direction :down))

(defparameter *test-soldier2* (make-soldier :pos (cons 2 4)
                                            :health 3
                                            :type :assassin
                                            :team "player2"
                                            :destination (cons 3 1)
                                            :attack-direction :down))

(defparameter *test-soldier3* (make-soldier :pos (cons 4 4)
                                            :health 3
                                            :type :assassin
                                            :team "player1"
                                            :destination (cons 3 1)
                                            :attack-direction :down))

(defparameter *test-soldier4* (make-soldier :pos (cons 3 3)
                                            :health 3
                                            :type :assassin
                                            :team "player2"
                                            :destination (cons 3 1)
                                            :attack-direction :down))

(defparameter *test-soldier5* (make-soldier :pos (cons 3 5)
                                            :health 3
                                            :type :assassin
                                            :team "player2"
                                            :destination (cons 3 1)
                                            :attack-direction :down))

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
      (bind (((x . y) (soldier-pos *test-soldier1*))
             (reachable (reachable-positions 3 (soldier-pos *test-soldier1*) mp)))
        (loop for k being the hash-keys of reachable do (setf (gethash k reachable) t))
        (ok (equalp 
             reachable
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
  (testing "doesn't find positions that are too far away based on distance travelled"
    (let* ((soldier (make-soldier :pos (cons 0 1) 
                                  :health 4
                                  :type :scout
                                  :team "player1"
                                  :destination (cons -3 0)
                                  :attack-direction :down))
           (mp (alist-hash-table (list (cons (cons 0 0) (make-rock))
                                       (cons (cons 1 0) soldier)) :test 'equal))
           (reachable (reachable-positions 3 (cons 1 0) mp)))
      (ok (not (gethash (cons -1 0) reachable)))
      (ok (not (gethash (cons -2 0) reachable)))))
  (testing "doesn't find nearby positions that are blocked"
    (let ((mp (alist-hash-table (append
                                 (list (cons (cons 1 1) (make-rock))
                                       (cons (cons 1 7) (make-rock)))
                                 (mapcar (lambda (s) (cons (soldier-pos s) s))
                                         (list *test-soldier1* *test-soldier2*
                                               *test-soldier3* *test-soldier4*
                                               *test-soldier5*))) :test 'equal)))
      (bind (((x . y) (soldier-pos *test-soldier1*))
             (reachable (reachable-positions 3 (soldier-pos *test-soldier1*) mp)))
        (loop for k being the hash-keys of reachable do (setf (gethash k reachable) t))
        (ok (equalp reachable
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
                                               (cons x (+ y 1))
                                               (cons x (+ y 2))
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
      (ok (equal (closest-reachable-position *test-soldier1* mp *default-game-config*)
                 (cons 2 5)))))
  (testing "finds the closest position within the travel distance of a soldier"
    (let* ((soldier (make-soldier :pos (cons 5 0)
                                 :health 4
                                 :type :scout
                                 :team "player1"
                                 :destination (cons 0 0)
                                 :attack-direction :down))
           (mp (alist-hash-table (list (cons (cons 5 0) soldier)
                                       (cons (cons 4 0) (make-rock)))
                                :test 'equal)))
      (ok (equal (closest-reachable-position soldier mp *default-game-config*)
                 (cons 3 -1)))))
  (testing "chooses the least lexicographic position when multiple closest positions exist"
    (let ((new-soldier1 (copy-structure *test-soldier1*)))
      (setf (soldier-destination new-soldier1) (cons 2 1))
      (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                          (list new-soldier1 *test-soldier2*
                                                *test-soldier3* *test-soldier4*
                                                *test-soldier5*))
                                  :test 'equal)))
        (ok (equal (closest-reachable-position new-soldier1 mp *default-game-config*)
                   (cons 1 1)))))))

(deftest move-soldier
  (testing "moves a soldier to the reachable position closest to it's destination"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier1* *test-soldier2*
                                              *test-soldier3* *test-soldier4*
                                              *test-soldier5*))
                                 :test 'equal)))
      (move-soldier mp *test-soldier1* *default-game-config*)
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
        (move-soldiers mp *default-game-config* 10)
        (ok (equalp mp
                    (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                              (list new-soldier1 new-soldier2)) :test 'equal))))))
  (testing "moves soldiers in lexicographical order of their starting coordinate when they are on the same team"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier2* *test-soldier4*)) :test 'equal)))
      (let* ((new-soldier2 (copy-structure *test-soldier2*))
             (new-soldier4 (copy-structure *test-soldier4*)))
        (setf (soldier-pos new-soldier2) (cons 2 1))
        (setf (soldier-pos new-soldier4) (cons 3 1))
        (move-soldiers mp *default-game-config* 10)
        (ok (equalp mp
                    (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                              (list new-soldier2 new-soldier4)) :test 'equal))))))
  (testing "moves soldiers from player1 first when there are an even number of turns remaining"
    (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier2* *test-soldier3*)) :test 'equal)))
      (let* ((new-soldier2 (copy-structure *test-soldier2*))
             (new-soldier3 (copy-structure *test-soldier3*)))
        (setf (soldier-pos new-soldier2) (cons 2 1))
        (setf (soldier-pos new-soldier3) (cons 3 1))
        (move-soldiers mp *default-game-config* 10)
        (ok (equalp mp
                    (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                              (list new-soldier2 new-soldier3)) :test 'equal))))))
  (testing "moves soldiers from player2 first when there are an odd number of turns remaining"
    (let* ((player1-soldier (make-soldier :pos (cons 1 4)
                                          :health 4
                                          :type :assassin
                                          :team "player1"
                                          :destination (cons 1 5)
                                          :attack-direction :down))
           (player2-soldier (make-soldier :pos (cons 2 6)
                                          :health 4
                                          :type :assassin
                                          :team "player2"
                                          :destination (cons 1 5)
                                          :attack-direction :down))
           (mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                         (list player1-soldier player2-soldier)) :test 'equal)))
      (let* ((new-soldier1 (copy-structure player1-soldier))
             (new-soldier2 (copy-structure player2-soldier)))
        (setf (soldier-pos new-soldier1) (cons 1 4))
        (setf (soldier-pos new-soldier2) (cons 1 5))
        (move-soldiers mp *default-game-config* 11)
        (ok (equalp mp
                    (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                              (list new-soldier1 new-soldier2)) :test 'equal))))))
  (testing "does nothing when there are no soldiers to move"
    (let ((mp (alist-hash-table (list (cons (cons 1 2) (make-rock)) 
                                      (cons (cons 3 4) *test-base1*)
                                      (cons (cons 6 9) *test-base2*)) :test 'equal))
          (expected (alist-hash-table (list (cons (cons 1 2) (make-rock)) 
                                      (cons (cons 3 4) *test-base1*)
                                      (cons (cons 6 9) *test-base2*)) :test 'equal)))
      (move-soldiers mp *default-game-config* 10)
      (ok (equalp mp expected)))))

(deftest eligible-target 
  (testing "returns false for a soldier from the same team as the attacker"
    (ok (not (eligible-target *test-soldier2* *test-soldier4*))))
  (testing "returns true for a soldier from the opposing team"
    (ok (eligible-target *test-soldier1* *test-soldier2*)))
  (testing "returns false for the base from a soldier's team"
    (ok (not (eligible-target *test-soldier1* *test-base1*))))
  (testing "returns true for the base from an enemy soldier's team"
    (ok (eligible-target *test-soldier1* *test-base2*)))
  (testing "returns false for a rock"
    (ok (not (eligible-target *test-soldier1* (make-rock))))))

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
        (ok (equalp (find-target *test-soldier1* mp) new-soldier4)))))
  (testing "ignores rocks"
    (let ((mp (alist-hash-table (list (cons (cons 3 4) (make-rock)) 
                                      (cons (soldier-pos *test-soldier2*) *test-soldier2*)
                                      (cons (cons 2 5) (make-rock))
                                      (cons (cons 1 4) (make-rock))
                                      (cons (cons 2 3) (make-rock))) :test 'equal)))
        (ok (not (find-target *test-soldier2* mp))))))

(deftest attack-soldier 
  (testing "decreases the health of a soldier by the difference between armour and damage"
    (let ((new-soldier2 (copy-structure *test-soldier2*))
          (mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                        (list *test-soldier1* *test-soldier2*)) :test 'equal)))
      (setf (soldier-health new-soldier2) 1)
      (attack-soldier *test-soldier1* *test-soldier2* mp *default-game-config*)
      (ok (equalp mp
                  (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                            (list *test-soldier1* new-soldier2)) :test 'equal)))))
  (testing "removes a soldier from the map when it's health reaches zero"
    (let ((new-soldier2 (copy-structure *test-soldier2*)))
      (setf (soldier-health new-soldier2) 2)
      (let ((mp (alist-hash-table (mapcar (lambda (s) (cons (soldier-pos s) s))
                                          (list *test-soldier1* new-soldier2)) :test 'equal)))
        (attack-soldier *test-soldier1* new-soldier2 mp *default-game-config*)
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
      (attack-soldier *test-soldier1* *test-soldier2* mp-copy (game-config gm))
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
                                       (cons (cons 10 10) (make-rock))
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
                                       (cons (cons 10 10) (make-rock))
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))))))
  (testing "makes soldiers attack simultaneously"
    (let* ((soldier1 (make-soldier :pos (cons 2 4)
                                   :health 4
                                   :type :assassin
                                   :team "player1"
                                   :destination (cons 2 4)
                                   :attack-direction :down))
           (soldier2 (make-soldier :pos (cons 1 4)
                                   :health 4
                                   :type :assassin
                                   :team "player2"
                                   :destination (cons 1 4)
                                   :attack-direction :down))
           (mp (alist-hash-table (list (cons (soldier-pos soldier1) soldier1)
                                       (cons (soldier-pos soldier2) soldier2)
                                       (cons (cons 10 1) *test-base1*)
                                       (cons (cons 10 5) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 10 :base (cons 4 3) :health 20))
           (player2 (make-player :team "player2" :money 7 :base (cons 5 4) :health 10))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (new-gm (make-soldiers-attack gm)))
      (ok (equalp (game-map new-gm)
                  (alist-hash-table (list (cons (cons 10 1) *test-base1*)
                                          (cons (cons 10 5) *test-base2*)) :test 'equal))))))

(deftest close-enough-to-base 
  (let ((player (make-player :team "player1" :money 10 :base (cons 4 3) :health 20)))
    (testing "is true when a position is closer than the max distance from a player's base"
      (ok (close-enough-to-base (cons 2 6) player *default-game-config*)))
    (testing "is false when a position is further than the max distance from a player's base"
      (ok (not (close-enough-to-base (cons 2 7) player *default-game-config*))))))

(deftest build-soldier
  (testing "fails when a soldier costs more than a player's money"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 9 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))) 
      (ok (equalp (build-soldier "player1" :scout (cons 4 4) (cons 8 10) gm)
                  (left "Player player1 with money 9 doesn't have enough money for SCOUT with cost 10")))))
  (testing "fails when the build position is already occupied"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 4 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (ok (equalp (build-soldier "player1" :scout (cons 4 4) (cons 8 10) gm)
                  (left "Player player1 tried to build SCOUT at position (4, 4) which is occupied")))))
  (testing "fails when the build position is too far from the player's base"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (ok (equalp (build-soldier "player1" :scout (cons 6 7) (cons 8 10) gm)
                  (left "Player player1 tried to build SCOUT at position (6, 7), too far from base (4, 3)")))))
  (testing "adds a soldier to the map and reduces the player's money"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (result (build-soldier "player1" :scout (cons 5 6) (cons 8 10) gm))
           (new-soldier (make-soldier :pos (cons 5 6) :health 6 
                                      :type :scout :team "player1"
                                      :destination (cons 8 10)
                                      :attack-direction :down))
           (new-player1 (make-player :team "player1" :money 10 :base (cons 4 3) :health 25))
           (new-mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                           (cons (cons 5 4) *test-base2*)
                                           (cons (cons 5 6) new-soldier)) :test 'equal))
           (new-gm (make-game :map new-mp :turns-remaining 20 :player1 new-player1 :player2 player2)))
      (ok (equalp result (right new-gm))))))

(deftest change-soldier-destination
  (testing "fails when the soldier position is not occupied"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (ok (equalp (change-soldier-destination "player1" (cons 1 2) (cons 5 6) gm)
                  (left "Player player1 tried to change destination, but position (1, 2) is not occupied")))))
  (testing "fails when the position is occupied by a base instead of a soldier"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (ok (equalp (change-soldier-destination "player1" (cons 4 3) (cons 5 6) gm)
                  (left "Player player1 tried to change destination, but position (4, 3) has a base, not a soldier")))))
  (testing "fails when the position is occupied by an enemy soldier"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (soldier-pos *test-soldier2*) *test-soldier2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2)))
      (ok (equalp (change-soldier-destination "player1" (soldier-pos *test-soldier2*) (cons 5 6) gm)
                  (left (format nil "Player player1 tried to change destination, but position ~a has an enemy soldier" (format-position (soldier-pos *test-soldier2*))))))))
  (testing "succeeds when the position is occupied by a soldier from the player's team"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (soldier-pos *test-soldier1*) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (new-map (copy-hash-table (game-map gm)))
           (new-gm (copy-structure gm))
           (new-soldier (copy-structure *test-soldier1*)))
      (setf (game-map new-gm) new-map)
      (setf (soldier-destination new-soldier) (cons 10 10))
      (setf (gethash (soldier-pos new-soldier) new-map) new-soldier)
      (ok (equalp (change-soldier-destination "player1" (soldier-pos *test-soldier1*) (cons 10 10) new-gm)
                  (right new-gm))))))

(deftest format-command
  (testing "prints a no-op the same way it was read in"
    (ok (equalp (fmap (lambda (res) (format-command (cdr res)))
                      (parse-move (cons "player1" "NO-OP")))
                (right "NO-OP"))))
  (testing "prints a build command the same way it was read in"
    (ok (equalp (fmap (lambda (res) (format-command (cdr res)))
                      (parse-move (cons "player1" "BUILD ASSASSIN (1, 2) (5, 3)")))
                (right "BUILD ASSASSIN (1, 2) (5, 3)"))))
  (testing "prints a change destination command the same way it was read in"
    (ok (equalp (fmap (lambda (res) (format-command (cdr res)))
                      (parse-move (cons "player1" "MOVE (1, 4) TO (6, 9)")))
                (right "MOVE (1, 4) TO (6, 9)")))))

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
                                      :type :scout :team "player1"
                                      :destination (cons 8 10)
                                      :attack-direction :down))
           (new-player1 (make-player :team "player1" :money 10 :base (cons 4 3) :health 25))
           (new-mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                           (cons (cons 5 4) *test-base2*)
                                           (cons (cons 5 6) new-soldier)) :test 'equal))
           (new-gm (make-game :map new-mp :turns-remaining 20 :player1 new-player1 :player2 player2)))
      (ok (equalp result (right new-gm)))))
  (testing "changes a soldier's destination when the command is change destination"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (soldier-pos *test-soldier1*) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (new-map (copy-hash-table (game-map gm)))
           (new-gm (copy-structure gm))
           (new-soldier (copy-structure *test-soldier1*)))
      (setf (game-map new-gm) new-map)
      (setf (soldier-destination new-soldier) (cons 10 10))
      (setf (gethash (soldier-pos new-soldier) new-map) new-soldier)
      (ok (equalp (apply-move "player1"
                              (make-change-destination :soldier-position (soldier-pos *test-soldier1*)
                                                       :new-destination (cons 10 10))
                              new-gm)
                  (right new-gm))))))

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
                  (make-move-result 
                   :errors (list "Player player1 tried to build SCOUT at position (4, 4) which is occupied" 
                                 "Player player2 with money 3 doesn't have enough money for ASSASSIN with cost 14")
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
                                                               :destination (cons 8 10)
                                                               :attack-direction :down))
                                           (cons (cons 3 3) 
                                                 (make-soldier :pos (cons 3 3)
                                                               :health 6
                                                               :type :assassin
                                                               :team "player2"
                                                               :destination (cons 9 6)
                                                               :attack-direction :down))) 
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
  (testing "is true when there is a disqualified bot"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)
                                       (cons (cons 1 4) *test-soldier1*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm1 (make-game :map mp :turns-remaining 10 
                           :player1 player1 :player2 player2
                           :disqualified-players (list "player1")))
           (gm2 (make-game :map mp :turns-remaining 10
                           :player1 player1 :player2 player2
                           :disqualified-players (list "player2")))
           (gm3 (make-game :map mp :turns-remaining 10
                           :player1 player1 :player2 player2
                           :disqualified-players (list "player1" "player2"))))
      (ok (game-over gm1))
      (ok (game-over gm2))
      (ok (game-over gm3))))
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
  (testing "returns win for player1 when player2 has been disqualified"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 10))
           (gm (make-game :map mp :turns-remaining 10 
                          :player1 player1 :player2 player2
                          :disqualified-players (list "player2"))))
      (ok (equalp (determine-result gm) "Winner player1"))))
  (testing "returns win for player2 when player1 has been disqualified"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 10))
           (gm (make-game :map mp :turns-remaining 10 
                          :player1 player1 :player2 player2
                          :disqualified-players (list "player1"))))
      (ok (equalp (determine-result gm) "Winner player2"))))
  (testing "return draw when both players have been disqualified"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 10))
           (gm (make-game :map mp :turns-remaining 10 
                          :player1 player1 :player2 player2
                          :disqualified-players (list "player1" "player2"))))
      (ok (equalp (determine-result gm) "Draw"))))
  (testing "returns win for player1 when player2 has no health and player1 has non-zero health"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 0))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2)))
      (ok (equalp (determine-result gm) "Winner player1"))))
  (testing "returns win for player2 when player1 has no health and player2 has non-zero health"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 0))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 10 :player1 player1 :player2 player2)))
      (ok (equalp (determine-result gm) "Winner player2"))))
  (testing "returns draw otherwise"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 3 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 0 :player1 player1 :player2 player2)))
      (ok (equalp (determine-result gm) "Draw")))))

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
                                                               :destination (cons 5 5)
                                                               :attack-direction :down))
                                           (cons (cons 3 3) 
                                                 (make-soldier :pos (cons 3 3)
                                                               :health 6
                                                               :type :assassin
                                                               :team "player2"
                                                               :destination (cons 3 3)
                                                               :attack-direction :down))) 
                                     :test 'equal))
           (new-player1 (make-player :team "player1" 
                                     :money (+ 10 (money-per-turn *default-game-config*)) 
                                     :base (cons 4 3) :health 20))
           (new-player2 (make-player :team "player2" 
                                     :money (+ 1 (money-per-turn *default-game-config*)) 
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
           (map-repr (vector (list (cons "position" (coord-alist (cons 1 4)))
                                   (cons "health" 4)
                                   (cons "type" "Scout")
                                   (cons "team" "player1")
                                 (cons "destination" (coord-alist (cons 5 5))))
                             (list (cons "type" "Base")
                                   (cons "position" (coord-alist (cons 4 3)))
                                   (cons "team" "player1"))
                             (list (cons "type" "Base")
                                   (cons "position" (coord-alist (cons 5 4)))
                                   (cons "team" "player2"))))
           (player1-repr (list (cons "team" "player1")
                               (cons "money" 20)
                               (cons "base" (coord-alist (cons 4 3)))
                               (cons "health" 25)))
           (player2-repr (list (cons "team" "player2")
                               (cons "money" 3)
                               (cons "base" (coord-alist (cons 5 4)))
                               (cons "health" 7)))
           (game-repr (list 
                            (cons "map" map-repr) 
                            (cons "turns-remaining" 10)
                            (cons "player1" player1-repr)
                            (cons "player2" player2-repr)))
           (player-1-game-json (let ((yason:*list-encoder* #'yason:encode-alist))
                                 (yason:with-output-to-string* (:stream-symbol s) 
                                   (yason:encode (cons (cons "you" "player1") game-repr) s))))
           (player-2-game-json (let ((yason:*list-encoder* #'yason:encode-alist))
                                 (yason:with-output-to-string* (:stream-symbol s) 
                                   (yason:encode (cons (cons "you" "player2") game-repr) s))))
           (players-input (get-players-input-for-turn gm)))
      (ok (equalp players-input
                  (list (cons "player1" player-1-game-json)
                        (cons "player2" player-2-game-json)))))))

(deftest parse-move 
  (testing "parses a build move"
    (ok (equalp (parse-move (cons "player1" "BUILD SCOUT (1, 3) (4, 6)"))
                (right (cons "player1"
                             (make-build :soldier-type :scout
                                         :start (cons 1 3)
                                         :destination (cons 4 6)))))))
  (testing "only accepts three SCOUT, TANK or ASSASSIN for soldier type"
    (ok (equalp (parse-move (cons "player1" "BUILD SCOUT (1, 3) (4, 6)"))
                (right (cons "player1"
                             (make-build :soldier-type :scout
                                         :start (cons 1 3)
                                         :destination (cons 4 6))))))
    (ok (equalp (parse-move (cons "player1" "BUILD TANK (1, 3) (4, 6)"))
                (right (cons "player1"
                             (make-build :soldier-type :tank
                                         :start (cons 1 3)
                                         :destination (cons 4 6))))))
    (ok (equalp (parse-move (cons "player1" "BUILD ASSASSIN (1, 3) (4, 6)"))
                (right (cons "player1"
                             (make-build :soldier-type :assassin
                                         :start (cons 1 3)
                                         :destination (cons 4 6))))))
    (ok (equalp (parse-move (cons "player1" "BUILD INFANTRY (1, 3) (4, 6)"))
                (left "Player player1 provided invalid move 'BUILD INFANTRY (1, 3) (4, 6)'"))))
  (testing "parses a no-op move"
    (ok (equalp (parse-move (cons "player1" "NO-OP"))
                (right (cons "player1" :no-op)))))
  (testing "returns an error when the player didn't provide a move"
    (ok (equalp (parse-move (cons "player1" nil))
                (left "Player player1 didn't provide a move"))))
  (testing "returns an error for other input"
    (ok (equalp (parse-move (cons "player1" "blah"))
                (left "Player player1 provided invalid move 'blah'")))))

(deftest advance-turn 
  (testing "parses moves and steps the game"
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
           (mp-copy (copy-hash-table mp))
           (player1-copy (copy-structure player1))
           (player2-copy (copy-structure player2))
           (gm-copy (make-game :map mp-copy :turns-remaining 20 :player1 player1-copy :player2 player2-copy))
           (unparsed-moves (list (cons "player1" "BUILD SCOUT (6, 5) (5, 5)")
                                 (cons "player2" "BUILD ASSASSIN (3, 2) (3, 3)")))
           (step-result (step-game moves gm))
           (advance-turn-result (advance-turn unparsed-moves gm-copy nil)))
      (ok (equalp (move-result-updated-game step-result) (game-turn-result-game advance-turn-result)))))
  (testing "discards moves which failed parsing"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 15 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (moves (list (cons "player1" (make-build :soldier-type :scout
                                                    :start (cons 6 5)
                                                    :destination (cons 5 5)))))
           (mp-copy (copy-hash-table mp))
           (player1-copy (copy-structure player1))
           (player2-copy (copy-structure player2))
           (gm-copy (make-game :map mp-copy :turns-remaining 20 :player1 player1-copy :player2 player2-copy))
           (unparsed-moves (list (cons "player1" "BUILD SCOUT (6, 5) (5, 5)")
                                 (cons "player2" "BUILD  (3, 2) (3, 3)")))
           (step-result (step-game moves gm))
           (advance-turn-result (advance-turn unparsed-moves gm-copy nil)))
      (ok (equalp (move-result-updated-game step-result)
                  (game-turn-result-game advance-turn-result)))))
  (testing "sets disqualified players in the game state"
    (let* ((mp (alist-hash-table (list (cons (cons 4 3) *test-base1*)
                                       (cons (cons 5 4) *test-base2*)) :test 'equal))
           (player1 (make-player :team "player1" :money 20 :base (cons 4 3) :health 25))
           (player2 (make-player :team "player2" :money 15 :base (cons 5 4) :health 24))
           (gm (make-game :map mp :turns-remaining 20 :player1 player1 :player2 player2))
           (unparsed-moves (list (cons "player1" "NO-OP")
                                 (cons "player2" nil)))
           (updated-game (game-turn-result-game 
                          (advance-turn unparsed-moves (copy-structure gm) (list "player2")))))
      (ok (equalp (game-disqualified-players updated-game) (list "player2"))))))

(defparameter *test-base-path* (directory-namestring #.*compile-file-truename*))

(deftest construct-bot-paths
  (testing "ensures that user provided bot directories are treated as directories"
    (let ((bot-dir-1 "/bot1")
          (bot-dir-2 "/bot2"))
      (bind (((bot1-path . bot2-path) (construct-bot-paths (cons bot-dir-1 bot-dir-2))))
        (ok (string= (format nil "~a" bot1-path) "/bot1/"))
        (ok (string= (format nil "~a" bot2-path) "/bot2/")))))
  (testing "prepends the current directory when provided"
    (let ((bot-dir-1 "bot1/")
          (bot-dir-2 "bot2/"))
      (bind (((bot1-path . bot2-path) (construct-bot-paths (cons bot-dir-1 bot-dir-2) "/bots")))
        (ok (string= (format nil "~a" bot1-path) "/bots/bot1/"))
        (ok (string= (format nil "~a" bot2-path) "/bots/bot2/")))))
  (testing "resolves a relative path to an absolute path"
    (let ((bot-dir-1 "bot1")
          (bot-dir-2 "bot2"))
      (bind (((bot1-path . bot2-path) (construct-bot-paths (cons bot-dir-1 bot-dir-2) *test-base-path*)))
        (ok (string= (format nil "~a" bot1-path) (format nil "~abot1/" *test-base-path*)))
        (ok (string= (format nil "~a" bot2-path) (format nil "~abot2/" *test-base-path*)))))))

(deftest map-from-lines 
  (testing "should fail to parse a map with disallowed characters"
    (let ((lines (list "i")))
      (ok (equalp (map-from-lines lines)
                  (left "The provided map contains characters which are not allowed. Only '1', '2', 'X' and ' ' are allowed.")))))
  (testing "should fail to parse a map when player1's base is missing"
    (let ((lines (list "2")))
      (ok (equalp (map-from-lines lines)
                  (left "No base for player1 was found on the map")))))
  (testing "should fail to parse a map when player2's base is missing"
    (let ((lines (list "1")))
      (ok (equalp (map-from-lines lines)
                  (left "No base for player2 was found on the map")))))
  (testing "should successfully read a map with two bases"
    (let ((lines (list "1 2")))
      (ok (equalp (map-from-lines lines)
                  (right (make-map-details 
                          :base1 (cons 0 0) :base2 (cons 2 0)
                          :map (alist-hash-table (list (cons (cons 0 0)
                                                             (make-base :team "player1"))
                                                       (cons (cons 2 0) 
                                                             (make-base :team "player2")))
                                                 :test 'equal)))))))
  (testing "should fail to parse a map when there is more than one position for player1's base"
    (let ((lines (list "1 1 2")))
      (ok (equalp (map-from-lines lines)
                  (left "More than one base position was specified for player1")))))
  (testing "should fail to parse a map when there is more than one position for player2's base"
    (let ((lines (list "1 2 2")))
      (ok (equalp (map-from-lines lines)
                  (left "More than one base position was specified for player2")))))
  (testing "should read a map with rocks"
    (let ((lines (list "X 1 2 X")))
      (ok (equalp (map-from-lines lines)
                  (right (make-map-details 
                          :base1 (cons 2 0) :base2 (cons 4 0)
                          :map (alist-hash-table (list (cons (cons 0 0) (make-rock))
                                                       (cons (cons 2 0) 
                                                             (make-base :team "player1"))
                                                       (cons (cons 4 0)
                                                             (make-base :team "player2"))
                                                       (cons (cons 6 0) (make-rock))) 
                                                 :test 'equal)))))))
  (testing "should read a map with multiple rows"
    (let ((lines (list "X   X"
                       "1 X 2"
                       "X   X")))
      (ok (equalp (map-from-lines lines)
                  (right (make-map-details 
                          :base1 (cons 0 1) :base2 (cons 4 1)
                          :map (alist-hash-table (list (cons (cons 0 0) (make-rock))
                                                       (cons (cons 4 0) (make-rock))
                                                       (cons (cons 0 1) 
                                                             (make-base :team "player1"))
                                                       (cons (cons 2 1) (make-rock))
                                                       (cons (cons 4 1)
                                                             (make-base :team "player2"))
                                                       (cons (cons 0 2) (make-rock))
                                                       (cons (cons 4 2) (make-rock)))
                                                 :test 'equal))))))))

(deftest map-from-file 
  (testing "should read a game-map from a file"
    (let ((test-path (format nil "~atest.mp" *test-base-path*)))
      (with-open-file (f test-path :if-does-not-exist :create
                         :if-exists :supersede :direction :output)
        (when f (format f "X   X~%1 X 2~%X   X")))
      (let ((mp (with-open-file (f test-path) (when f (map-from-file f)))))
        (ok (equalp mp
                    (right (make-map-details 
                          :base1 (cons 0 1) :base2 (cons 4 1)
                          :map (alist-hash-table (list (cons (cons 0 0) (make-rock))
                                                       (cons (cons 4 0) (make-rock))
                                                       (cons (cons 0 1) 
                                                             (make-base :team "player1"))
                                                       (cons (cons 2 1) (make-rock))
                                                       (cons (cons 4 1)
                                                             (make-base :team "player2"))
                                                       (cons (cons 0 2) (make-rock))
                                                       (cons (cons 4 2) (make-rock)))
                                                 :test 'equal))))))      
      (let ((test-file (probe-file test-path)))
        (when test-file (delete-file test-file))))))

(deftest start-game
  (testing "runs bots and plays game until it is finished"
    (let ((result (start-game 
                   (cons "bot1/" "bot2/")
                   (make-logging-config :turns *standard-output*
                                        :moves nil
                                        :states nil
                                        :visualisation nil)
                   :current-directory (directory-namestring #.*compile-file-truename*)
                   :game-config (make-instance 
                                 'game-config
                                 :initial-money 10
                                 :money-per-turn 3
                                 :total-turns 100
                                 :allowed-commands 
                                 (alist-hash-table
                                  (list (cons "lisp-ros-herodotus"
                                              "ros -Q -s herodotus -- <bot-file>"))
                                  :test 'equal)
                                 :max-distance-from-base 5
                                 :bot-memory-limit-kib (bot-memory-limit-kib *default-game-config*)
                                 :bot-initialisation-time (bot-initialisation-time *default-game-config*)
                                 :health *default-health-config*
                                 :speed-config *default-speed-config*
                                 :damage *default-damage-config*
                                 :cost *default-cost-config*))))
      (ok (equalp (fmap #'determine-result result) (right "Winner player2"))))))
