(defpackage runtime/tests/runtime
  (:use :cl
        :runtime
        :rove))

(in-package :runtime/tests/runtime)

;; NOTE: To run this test file, execute `(asdf:test-system :runtime)' in your Lisp.

(defparameter *quick-bot* 
  (format nil "~a" (merge-pathnames (directory-namestring #.*compile-file-truename*) "quick-bot.lisp")))

(defparameter *slow-bot*
  (format nil "~a" (merge-pathnames (directory-namestring #.*compile-file-truename*) "slow-bot.lisp")))

(deftest bot-output
  (testing "should capture bot output from the commandline"
    (let ((bot (run-bot "sbcl" (list "--script" *quick-bot*))))
      (sleep 0.05)
      (ok (string= (bot-output bot)
                   (format nil "~a~%" "bot output"))))))

(deftest start-bot
  (testing "should start the bot process"
    (let ((bot (run-bot "sbcl" (list "--script" *slow-bot*))))
      (ok (equal (bot-status bot) :running))
      (interrupt-bot bot))))

(deftest suspend-bot
  (testing "should suspend a bot"
    (let ((bot (run-bot "sbcl" (list "--script" *slow-bot*))))
      (stop-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (interrupt-bot bot))))

(deftest continue-bot
  (testing "should continue execution of a bot"
    (let ((bot (run-bot "sbcl" (list "--script" *slow-bot*))))
      (stop-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (continue-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :running))
      (interrupt-bot bot))))

(deftest interrupt-bot 
  (testing "should interrupt execution of a bot"
    (let ((bot (run-bot "sbcl" (list "--script" *slow-bot*))))
      (sleep 0.01)
      (interrupt-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :exited)))))
