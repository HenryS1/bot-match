(defpackage footsoldiers/tests/main
  (:use :cl
        :footsoldiers
        :rove))
(in-package :footsoldiers/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :footsoldiers)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
