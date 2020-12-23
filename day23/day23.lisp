(defun circle (xs)
  (let ((ys (copy-tree xs)))
    (setf (cdr (last ys)) ys)))

(defun take (n xs)
  (if (> n 0) (cons (car xs) (take (- n 1) (cdr xs)))
              nil))

(defun drop (n xs)
  (if (> n 0) (drop (- n 1) (cdr xs))
              xs))

(defun circle-drop-impl (xs old-start new-start)
  (if (eq (cdr xs) old-start) (setf (cdr xs) new-start)
                              (circle-drop-impl (cdr xs) old-start new-start)))

(defun circle-drop (n xs)
  (let ((ys (drop n xs)))
    (circle-drop-impl ys xs ys)))

(defun main ()
  (setq *print-circle* t)
  (let ((part1 (circle (list 9 1 6 4 3 8 2 7 5))))
    (format t "Part 1: ~S~%" part1)))
