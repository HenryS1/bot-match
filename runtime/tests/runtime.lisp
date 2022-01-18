(defpackage runtime/tests/runtime
  (:use :cl
        :runtime
        :rove))

(in-package :runtime/tests/runtime)

(defparameter *test-base-path* (directory-namestring #.*compile-file-truename*))

(defun test-file-path (filename)
  (format nil "~a" (merge-pathnames *test-base-path* filename)))

(defparameter *quick-bot* (test-file-path "quick-bot.lisp"))

(defparameter *slow-bot* (test-file-path "slow-bot.lisp"))

(defparameter *input-bot* (test-file-path "input-bot.lisp"))

(defparameter *turn-bot* (test-file-path "turn-bot.lisp"))

(defparameter *exited-bot* (test-file-path "exited-bot.lisp"))

(defparameter *bot-definition* (test-file-path "definition.json"))

(defun run-test-bot (path)
  (make-instance 'concrete-bot :bot-process (run-bot "ros" (list "+Q" "--" path)) :bot-id (random 100) :bot-name "test-bot"))

(defparameter *turn-timeout* 3)

(deftest bot-output
  (testing "should capture bot output from stdout"
    (let ((bot (run-test-bot *quick-bot*)))
      (sleep 0.2)
      (ok (equal (bot-output bot *turn-timeout*) '("bot output"))))))

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
      (send-input-to-bot bot (format nil "hello bot~%"))
      (let ((bot-out (bot-output bot *turn-timeout*)))
        (ok (equal bot-out '("hello bot")))
        (interrupt-bot bot)))))

(deftest end-bot-turn
  (testing "should stop a bot"
    (let ((bot (run-test-bot *turn-bot*)))
      (send-input-to-bot bot (format nil "input~%"))
      (end-bot-turn bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (interrupt-bot bot))))

(deftest bot-turn
  (testing "should send input, read output and stop bot"
    (let ((bot (run-test-bot *turn-bot*)))
      (sleep 0.5)
      (ok (equal (bot-turn bot (format nil "input~%") *turn-timeout*) '("input")))
      (ok (equal (bot-status bot) :stopped))
      (interrupt-bot bot)))
  (testing "should not run a turn when the bot process is exited"
    (let ((bot (run-test-bot *turn-bot*)))
      (sleep 0.2)
      (interrupt-bot bot)
      (sleep 0.02)
      (ok (not (bot-turn bot (format nil "input~%") *turn-timeout*))))))

(deftest bot-definition 
  (testing "should read a bot definition from a file"
    (with-open-file (f *bot-definition*)
      (let ((definition (bot-definition-json:from-json f)))
        (ok (equal (runtime:name definition) "bot"))
        (ok (equal (runtime:command definition) "ros +Q -- <bot-file>"))
        (ok (equal (runtime:relative-filepath definition) "./turn-bot.lisp"))))))

(deftest start-bot-from-definition 
  (testing "should start a bot process using the provided command"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (bot (start-bot-from-definition definition *test-base-path*)))
        (sleep 0.01)
        (ok (equal (bot-turn bot (format nil "input~%") *turn-timeout*) '("input")))))))
