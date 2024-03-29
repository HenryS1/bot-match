(defun read-input ()
  (parse-integer (loop for line = (read-line t nil nil)
                    while (listen)
                    finally (return line))))

(defun run ()
  (format t "Ready~%")
  (loop for input = (read-input)
     do (format t "~a~%" input)
       (finish-output)))

(defun main (&rest argv)
  (declare (ignorable argv))
  (run))
