(defun read-file (name)
  (with-open-file (stream name)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun main ()
  (let ((input (read-file "resources/input.txt")))
    (format t "~S~%" input)))
