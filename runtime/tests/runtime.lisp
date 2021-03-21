(defpackage runtime/tests/runtime
  (:use :cl
        :runtime
        :rove))

(in-package :runtime/tests/runtime)

;; NOTE: To run this test file, execute `(asdf:test-system :runtime)' in your Lisp.

(defun bot-path (filename)
  (format nil "~a" (merge-pathnames (directory-namestring #.*compile-file-truename*) filename)))

(defparameter *quick-bot* (bot-path "quick-bot.lisp"))

(defparameter *slow-bot* (bot-path "slow-bot.lisp"))

(defparameter *input-bot* (bot-path "input-bot.lisp"))

(defparameter *turn-bot* (bot-path "turn-bot.lisp"))

(defun run-test-bot (path)
  (run-bot "sbcl" (list "--script" path)))

(deftest bot-output
  (testing "should capture bot output from the commandline"
    (let ((bot (run-test-bot *quick-bot*)))
      (sleep 0.05)
      (ok (equal (bot-output bot) '("bot output"))))))

(deftest start-bot
  (testing "should start the bot process"
    (let ((bot (run-test-bot *slow-bot*)))
      (ok (equal (bot-status bot) :running))
      (interrupt-bot bot))))

(deftest suspend-bot
  (testing "should suspend a bot"
    (let ((bot (run-test-bot *slow-bot*)))
      (stop-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (interrupt-bot bot))))

(deftest continue-bot
  (testing "should continue execution of a bot"
    (let ((bot (run-test-bot *slow-bot*)))
      (stop-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (continue-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :running))
      (interrupt-bot bot))))

(deftest interrupt-bot
  (testing "should interrupt execution of a bot"
    (let ((bot (run-test-bot *slow-bot*)))
      (sleep 0.1)
      (interrupt-bot bot)
      (sleep 0.1)
      (ok (equal (bot-status bot) :exited)))))

(deftest send-input-to-bot
  (testing "should send input to a bot"
    (let ((bot (run-test-bot *input-bot*)))
      (send-input-to-bot bot "hello bot")
      (sleep 0.01)
      (ok (equal (bot-output bot) '("hello bot")))
      (interrupt-bot bot))))

(deftest end-bot-turn
  (testing "should stop a bot and read its output"
    (let ((bot (run-test-bot *turn-bot*)))
      (send-input-to-bot bot "input")
      (sleep 0.01)
      (end-bot-turn bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (interrupt-bot bot))))

(deftest bot-turn
  (testing "should send input, read output and stop bot"
    (let ((bot (run-test-bot *turn-bot*)))
      (sleep 0.01)
      (ok (equal (bot-turn bot "input" 0.02) '("input")))
      (ok (equal (bot-status bot) :stopped))
      (interrupt-bot bot))))
