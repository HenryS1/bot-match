(defpackage runtime/tests/runtime
  (:use :cl
        :runtime
        :rove))

(in-package :runtime/tests/runtime)

;; NOTE: To run this test file, execute `(asdf:test-system :runtime)' in your Lisp.

(defparameter *test-directory* 
  (format nil "~a" (merge-pathnames (directory-namestring #.*compile-file-truename*) "dummy-bot.lisp")))

(deftest run-bot
  (testing "should capture bot output from the commandline"
    (ok (= (run-bot "sbcl" (list "--script" *test-directory*)) "bot-output"))))
