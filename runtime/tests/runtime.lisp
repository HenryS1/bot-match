(defpackage runtime/tests/runtime
  (:use :cl
        :runtime
        :cl-ppcre
        :trivia
        :either
        :docker-client
        :alexandria
        :monad
        :rove))

(in-package :runtime/tests/runtime)

(defparameter *test-base-path* (directory-namestring #.*compile-file-truename*))

(defun test-file-path (filename)
  (format nil "~a" (merge-pathnames *test-base-path* filename)))

(defun create-bot (bot-file)
  (let* ((directory-mount (format nil "~a:/bots" *test-base-path*))
         (host-config (make-instance 'host-config 
                                     :binds (list directory-mount)))
         (config (make-instance 'docker-config
                                :image "bot-match/lisp-base"
                                :cmd (split "\\s+" (format nil "ros +Q -- /bots/~a" bot-file))
                                :entrypoint (list "")
                                :open-stdin t
                                :volumes (alist-hash-table (list (cons "/bots" (alist-hash-table (list) :test 'equal))) :test 'equal)
                                :host-config host-config)))
    (create-container bot-file :docker-config config)))

(defparameter *bot-files* (list "quick-bot.lisp" "slow-bot.lisp" 
                           "error-bot.lisp" "input-bot.lisp"
                           "turn-bot.lisp" "exited-bot.lisp"
                           "filesystem-bot.lisp" "not-ready-bot.lisp"))

(setup (mapc #'create-bot *bot-files*))

(teardown
  (handler-case (progn (mapc #'stop-container *bot-files*)
                       (mapc #'remove-container *bot-files*))
    (error (e) (format t "Error during tear down ~a~%" e))))

(defparameter *bot-definition* (test-file-path "definition.json"))

(defparameter *filesystem-bot-definition* (test-file-path "filesystem-bot-definition.json"))

(defparameter *not-ready-bot-definition* (test-file-path "not-ready-bot-definition.json"))

(defun run-test-bot (identifier &key (bot-definition nil) (error-stream *error-output*))
  (make-instance 'concrete-bot
                 :bot-process (run-bot identifier) 
                 :bot-id (random 100) 
                 :bot-name identifier
                 :bot-definition bot-definition
                 :command nil
                 :error-stream error-stream))

(defmacro with-test-bot (var identifier bot-definition error-stream &rest body)
  (with-gensyms (bot-process)
    `(let* ((,bot-process (run-bot ,identifier))
            (,var (make-instance 'concrete-bot
                                 :bot-process ,bot-process
                                 :bot-id (random 100)
                                 :bot-name ,identifier
                                 :bot-definition ,bot-definition
                                 :command nil
                                 :error-stream ,error-stream)))
       (unwind-protect 
            ,@body
         (kill-bot ,var)))))

(defparameter *turn-timeout* 3)

(deftest bot-output
  (testing "should capture bot output from stdout"
    (let ((logs (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t)))
      (with-test-bot bot "quick-bot.lisp" nil *error-output*
                     (ok (equalp (with-output-to-string (s logs) (bot-output bot *turn-timeout* s)) 
                                 '("bot output")))
                     (ok (equalp logs (format nil "Bot quick-bot.lisp returned output (bot output)~%")))))))

(deftest start-bot
  (testing "should start the bot process"
    (with-test-bot bot "slow-bot.lisp" nil *error-output*
      (ok (running (bot-status bot)))
      (pause-bot bot))))

(deftest suspend-bot
  (testing "should suspend a bot"
    (with-test-bot bot "slow-bot.lisp" nil *error-output*
      (pause-bot bot)
      (ok (paused (bot-status bot)))
      (pause-bot bot))))

#+linux
(deftest memory-limiting
  (testing "should prevent a bot from starting if it exceeds the memory limit"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (initial-bot (start-bot-from-definition definition 
                                                     *test-base-path*
                                                     *standard-output*
                                                     *error-output*
                                                     :memory-limit 10)))
        (unwind-protect (progn (sleep 0.01)
                               (ok (not (running bot))))
          (fmap #'kill-bot initial-bot))))))

(deftest process-error-output
  (testing "should send error output to the error-stream stream provided in bot initialisation"
    (ok (equal "Bot error output" 
               (with-output-to-string (s)
                   (with-test-bot bot "error-bot.lisp" nil s
                     (sleep 0.3)
                     (process-bot-error-output bot nil)))))))

(deftest readiness-check
  (testing "should kill a bot if it fails to respond with ready within the time limit"
    (with-open-file (f *not-ready-bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (bot (start-bot-from-definition definition *test-base-path* 
                                             *standard-output* *error-output*)))
        (unwind-protect (ok (not (running (bot-status (right-value bot)))))
          (fmap #'kill-bot bot))))))

(deftest continue-bot
  (testing "should continue execution of a bot"
    (with-test-bot bot "slow-bot.lisp" nil *error-output*
      (pause-bot bot)
      (ok (paused (bot-status bot)))
      (continue-bot bot *standard-output*)
      (sleep 0.01)
      (ok (running (bot-status bot)))
      (interrupt-bot bot)))

  (testing "should restart the bot if it was killed"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (initial-bot (start-bot-from-definition definition *test-base-path*
                                                     *standard-output* *error-output*)))
        (unwind-protect 
             (match initial-bot 
               ((left (left-err e)) (fail (format nil "~a" e)))
               ((right (right-value bot))
                (sleep 0.01)
                (ok (paused (bot-status bot)))
                (kill-bot bot)
                (sleep 0.2)
                (ok (not (running (bot-status bot))))
                (let ((continued-bot (continue-bot bot *standard-output*)))
                  (unwind-protect
                       (progn (sleep 0.01)
                              (ok (running (bot-status continued-bot))))
                    (kill-bot continued-bot)))))
          (fmap #'kill-bot initial-bot)))))

  (testing "should not restart a killed bot if it has reached the maximum restarts"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (initial-bot (start-bot-from-definition definition *test-base-path*
                                                     *standard-output* *error-output*)))
        (unwind-protect 
             (match initial-bot
               ((left (left-err e)) (fail (format nil "~a" e)))
               ((right (right-value bot))
                (sleep 0.01)
                (ok (paused (bot-status bot)))
                (kill-bot bot)
                (setf (bot-restarts bot) *max-bot-restarts*)
                (sleep 0.2)
                (ok (not (running (bot-status bot))))
                (let ((continued-bot (continue-bot bot *standard-output*)))
                  (unwind-protect
                       (progn (sleep 0.01)
                              (ok (not (running (bot-status continued-bot)))))
                    (kill-bot continued-bot)))))
          (fmap #'kill-bot initial-bot)))))

  (testing "should restart the bot if it exited"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f)))
        (setf (name definition) "exited-bot.lisp")
        (let ((initial-bot (start-bot-from-definition definition *test-base-path*
                                                      *standard-output* *error-output*)))
          (unwind-protect (match initial-bot
             ((left (left-err e)) (format nil "~a" e))
             ((right (right-value bot))
              (sleep 0.01)
              (continue-bot bot *standard-output*)
              (sleep 0.5)
              (ok (has-exited bot))
              (setf (bot-name bot) "turn-bot.lisp")
              (let ((continued-bot (continue-bot bot *standard-output*)))
                (unwind-protect
                     (progn (sleep 0.01)
                            (ok (running (bot-status continued-bot))))
                 (kill-bot continued-bot)))))
            (fmap #'kill-bot initial-bot))))))
  (testing "should not restart an exited bot if it has reached the maximum restarts"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f)))
        (setf (name definition) "exited-bot.lisp")
        (let ((initial-bot (start-bot-from-definition definition *test-base-path*
                                                      *standard-output* *error-output*)))
          (unwind-protect (match initial-bot
             ((left (left-err e)) (fail (format nil "~a" e)))
             ((right (right-value bot))
              (sleep 0.01)
              (continue-bot bot *standard-output*)
              (sleep 0.5)
              (ok (has-exited bot))
              (setf (command bot) "turn-bot.lisp")
              (setf (bot-restarts bot) *max-bot-restarts*)
              (let ((continued-bot (continue-bot bot *standard-output*)))
                (unwind-protect (progn (sleep 0.01)
                                       (ok (has-exited continued-bot)))
                  (kill-bot continued-bot)))))
            (fmap #'kill-bot initial-bot)))))))

(deftest interrupt-bot
  (testing "should interrupt execution of a bot"
    (with-test-bot bot "slow-bot.lisp" nil *error-output*
      (sleep 0.1)
      (interrupt-bot bot)
      (sleep 0.1)
      (ok (has-exited bot)))))

(deftest send-input-to-bot
  (testing "should send input to a bot"
    (with-test-bot bot "input-bot.lisp" nil *error-output*
      (send-input-to-bot bot (format nil "hello bot~%"))
      (let* ((logs (make-array 0 :element-type 'base-char :fill-pointer 0 :adjustable t))
             (bot-out (with-output-to-string (s logs) (bot-output bot *turn-timeout* s))))
        (ok (equal bot-out '("hello bot")))
        (ok (equal logs (format nil "Bot input-bot.lisp returned output (hello bot)~%")))
        (interrupt-bot bot)))))

(deftest end-bot-turn
  (testing "should stop a bot"
    (with-test-bot bot "turn-bot.lisp" nil *error-output*
      (send-input-to-bot bot (format nil "input~%"))
      (end-bot-turn bot nil)
      (sleep 0.01)
      (ok (paused (bot-status bot)))
      (interrupt-bot bot))))

(deftest bot-turn
  (testing "should send input, read output and stop bot"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (started-bot (start-bot-from-definition definition *test-base-path*
                                                     *standard-output* *error-output*)))
        (unwind-protect 
             (match started-bot 
               ((left (left-err e)) (fail (format nil "~a" e)))
               ((right (right-value bot))
                (sleep 0.5)
                (ok (equalp (bot-turn bot (format nil "input~%") *turn-timeout*) 
                            (make-bot-turn-result :updated-bot bot :output '("input"))))
                (ok (paused (bot-status bot)))
                (interrupt-bot bot)))
          (fmap #'kill-bot started-bot))))))

(deftest bot-definition 
  (testing "should read a bot definition from a file"
    (with-open-file (f *bot-definition*)
      (let ((definition (bot-definition-json:from-json f)))
        (ok (equal (runtime:name definition) "turn-bot.lisp"))
        (ok (equal (runtime:command definition) "lisp-ros"))
        (ok (equal (runtime:filename definition) "turn-bot.lisp"))))))

(deftest start-bot-from-definition 
  (testing "should start a bot process using the provided command"
    (with-open-file (f *bot-definition*)
      (let* ((definition (bot-definition-json:from-json f))
             (started-bot (start-bot-from-definition definition *test-base-path*
                                                     *standard-output* *error-output*)))
        (match started-bot
          ((left (left-err e)) (fail (format nil "~a" e)))
          ((right (right-value bot))
           (sleep 0.01)
           (ok (equalp (bot-turn bot (format nil "input~%") *turn-timeout*)
                       (make-bot-turn-result :updated-bot bot :output '("input"))))))))))
