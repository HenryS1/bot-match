(defpackage runtime/tests/runtime
  (:use :cl
        :runtime
        :rove))

(in-package :runtime/tests/runtime)

;; NOTE: To run this test file, execute `(asdf:test-system :runtime)' in your Lisp.

(defparameter *test-bot* 
  (format nil "~a" (merge-pathnames (directory-namestring #.*compile-file-truename*) "dummy-bot.lisp")))

(deftest run-bot-output
  (testing "should capture bot output from the commandline"
    (let ((bot (run-bot "sbcl" (list "--script" *test-bot*))))
      (sleep 1)
      (ok (string= (bot-output bot)
                   (format nil "~a~%" "bot output"))))))
