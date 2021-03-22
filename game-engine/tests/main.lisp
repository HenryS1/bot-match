(defpackage game-engine/tests/main
  (:use :cl
        :game-engine
        :rove))
(in-package :game-engine/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :game-engine)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
