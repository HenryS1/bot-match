(defpackage n-player-game/tests/main
  (:use :cl
        :n-player-game
        :rove))
(in-package :n-player-game/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :game-runner)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
