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

(defun run-test-bot (path)
  (run-bot "sbcl" (list "--script" path)))

(deftest bot-output
  (testing "should capture bot output from the commandline"
    (let ((bot (run-test-bot *quick-bot*)))
      (sleep 0.05)
      (ok (string= (bot-output bot) "bot output")))))

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
      (ok (string= (bot-output bot) "hello bot")))))
