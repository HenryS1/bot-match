(defun read-input ()
  (parse-integer (with-output-to-string (s)
     (loop for line = (read-line t nil nil)
        while (and line (> (length line) 0))
        do (write-line line s)))))

(defun run ()
  (loop for input = (read-input)
     do (format t "~a~%~%" (+ input 1))
       (finish-output)))

(run)
