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

(deftest run-bot-output
  (testing "should capture bot output from the commandline"
    (let ((bot (run-bot "sbcl" (list "--script" *quick-bot*))))
      (sleep 1)
      (ok (string= (bot-output bot)
                   (format nil "~a~%" "bot output"))))))
