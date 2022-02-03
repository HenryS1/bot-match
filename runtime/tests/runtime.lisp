(defpackage runtime/tests/runtime
  (:use :cl
        :runtime
        :ppcre
        :trivia
        :either
        :monad
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

(defparameter *filesystem-bot-definition* (test-file-path "filesystem-bot-definition.json"))

(defparameter *not-ready-bot-definition* (test-file-path "not-ready-bot-definition.json"))

(defun run-test-bot (path &optional (bot-definition nil))
  (make-instance 'concrete-bot
                 :bot-process (run-bot "ros" (list "+Q" "--" path)) 
                 :bot-id (random 100) 
                 :bot-name "test-bot"
                 :bot-definition bot-definition
                 :command nil))

(defparameter *turn-timeout* 3)

(deftest bot-output
  (testing "should capture bot output from stdout"
    (let ((logs (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t))
          (bot (run-test-bot *quick-bot*)))
      (sleep 0.2)
      (ok (equalp (with-output-to-string (s logs) (bot-output bot *turn-timeout* s)) 
                  '("bot output")))
      (ok (equalp logs (format nil "Bot test-bot returned output (bot output)~%"))))))

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

#+linux
(deftest memory-limiting
  (testing "should prevent a bot from starting if it exceeds the memory limit"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (initial-bot (start-bot-from-definition definition 
                                                     *test-base-path* *standard-output*
                                                     :memory-limit 10)))
        (sleep 0.01)
        (ok (equalp (fmap #'bot-status initial-bot) (right :exited)))))))

(deftest readiness-check
  (testing "should kill a bot if it fails to respond with ready within the time limit"
    (with-open-file (f *not-ready-bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (bot (start-bot-from-definition definition *test-base-path* *standard-output*)))
        (sleep 0.1)
        (ok (equalp (fmap #'bot-status bot) (right :signaled)))))))

(deftest file-access-limiting
  (testing "should prevent a bot from writing to a file"
    (with-open-file (f *filesystem-bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (bot (fmap (lambda (b) (continue-bot b *standard-output*)) 
                        (start-bot-from-definition 
                         definition
                         *test-base-path* *standard-output*))))
        (sleep 0.6)
        (let ((f (probe-file "./test-file")))
          (when f (delete-file f)))
        (ok (equalp (fmap #'bot-status bot) (right :exited)))))))

(deftest continue-bot
  (testing "should continue execution of a bot"
    (let ((bot (run-test-bot *slow-bot*)))
      (stop-bot bot)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (continue-bot bot *standard-output*)
      (sleep 0.01)
      (ok (equal (bot-status bot) :running))
      (interrupt-bot bot)))
  (testing "should restart the bot if it was killed"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (initial-bot (start-bot-from-definition definition *test-base-path* *standard-output*)))
        (match initial-bot 
          ((left (left-err e)) (fail (format nil "~a" e)))
          ((right (right-value bot))
           (sleep 0.01)
           (ok (equalp (bot-status bot) :stopped))
           (kill-bot bot)
           (sleep 0.2)
           (ok (equalp (bot-status bot) :signaled))
           (let ((continued-bot (continue-bot bot *standard-output*)))
             (sleep 0.01)
             (ok (equalp (bot-status continued-bot) :running))))))))
  (testing "should not restart a killed bot if it has reached the maximum restarts"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (initial-bot (start-bot-from-definition definition *test-base-path* *standard-output*)))
        (match initial-bot
          ((left (left-err e)) (fail (format nil "~a" e)))
          ((right (right-value bot))
           (sleep 0.01)
           (ok (equalp (bot-status bot) :stopped))
           (kill-bot bot)
           (setf (bot-restarts bot) *max-bot-restarts*)
           (sleep 0.2)
           (ok (equalp (bot-status bot) :signaled))
           (let ((continued-bot (continue-bot bot *standard-output*)))
             (sleep 0.01)
             (ok (equalp (bot-status continued-bot) :signaled))))))))
  (testing "should restart the bot if it exited"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f)))
        (setf (filename definition) "./exited-bot.lisp")
        (let ((initial-bot (start-bot-from-definition definition *test-base-path* *standard-output*)))
          (match initial-bot
            ((left (left-err e)) (format nil "~a" e))
            ((right (right-value bot))
             (sleep 0.01)
             (continue-bot bot *standard-output*)
             (sleep 0.5)
             (ok (equalp (bot-status bot) :exited))
             (setf (command bot) (regex-replace "exited-bot.lisp" 
                                                        (command bot)
                                                        "turn-bot.lisp"))
             (let ((continued-bot (continue-bot bot *standard-output*)))
               (sleep 0.01)
               (ok (equalp (bot-status continued-bot) :running)))))))))
  (testing "should not restart an exited bot if it has reached the maximum restarts"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f)))
        (setf (filename definition) "./exited-bot.lisp")
        (let ((initial-bot (start-bot-from-definition definition *test-base-path* *standard-output*)))
          (match initial-bot
            ((left (left-err e)) (fail (format nil "~a" e)))
            ((right (right-value bot))
             (sleep 0.01)
             (continue-bot bot *standard-output*)
             (sleep 0.5)
             (ok (equalp (bot-status bot) :exited))
             (setf (command bot) (regex-replace "exited-bot.lisp" 
                                                        (command bot)
                                                        "turn-bot.lisp"))
             (setf (bot-restarts bot) *max-bot-restarts*)
             (let ((continued-bot (continue-bot bot *standard-output*)))
               (sleep 0.01)
               (ok (equalp (bot-status continued-bot) :exited))))))))))

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
      (let* ((logs (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t))
             (bot-out (with-output-to-string (s logs) (bot-output bot *turn-timeout* s))))
        (ok (equal bot-out '("hello bot")))
        (ok (equal logs (format nil "Bot test-bot returned output (hello bot)~%")))
        (interrupt-bot bot)))))

(deftest end-bot-turn
  (testing "should stop a bot"
    (let ((bot (run-test-bot *turn-bot*)))
      (send-input-to-bot bot (format nil "input~%"))
      (end-bot-turn bot nil)
      (sleep 0.01)
      (ok (equal (bot-status bot) :stopped))
      (interrupt-bot bot))))

(deftest bot-turn
  (testing "should send input, read output and stop bot"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (started-bot (start-bot-from-definition definition *test-base-path* *standard-output*)))
        (match started-bot 
          ((left (left-err e)) (fail (format nil "~a" e)))
          ((right (right-value bot))
           (sleep 0.5)
           (ok (equalp (bot-turn bot (format nil "input~%") *turn-timeout*) 
                       (make-bot-turn-result :updated-bot bot :output '("input"))))
           (ok (equal (bot-status bot) :stopped))
           (interrupt-bot bot)))))))

(deftest bot-definition 
  (testing "should read a bot definition from a file"
    (with-open-file (f *bot-definition*)
      (let ((definition (bot-definition-json:from-json f)))
        (ok (equal (runtime:name definition) "bot"))
        (ok (equal (runtime:command definition) "lisp-ros"))
        (ok (equal (runtime:filename definition) "turn-bot.lisp"))))))

(deftest start-bot-from-definition 
  (testing "should start a bot process using the provided command"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (started-bot (start-bot-from-definition definition *test-base-path* *standard-output*)))
        (match started-bot
          ((left (left-err e)) (fail (format nil "~a" e)))
          ((right (right-value bot))
           (sleep 0.01)
           (ok (equalp (bot-turn bot (format nil "input~%") *turn-timeout*)
                       (make-bot-turn-result :updated-bot bot :output '("input"))))))))))
