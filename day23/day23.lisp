(defun circle (xs)
  (let ((ys (copy-tree xs)))
    (setf (cdr (last ys)) ys)))

(defun move (n cups))

(defun main ()
  (setq *print-circle* t)
  (let ((part1 (circle (list 9 1 6 4 3 8 2 7 5))))
    (format t "Part 1: ~S~%" part1)))
