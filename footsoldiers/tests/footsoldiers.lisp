(defpackage footsoldiers/tests/footsoldiers
  (:use :cl
        :footsoldiers
        :alexandria
        :metabang-bind
        :monad
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
