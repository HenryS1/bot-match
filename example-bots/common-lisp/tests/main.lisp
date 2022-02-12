(defpackage bot-match-bot/tests/main
  (:use :cl
        :bot-match-bot
        :rove))
(in-package :bot-match-bot/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :bot-match-bot)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
