(defun circle (xs)
  (let ((ys (copy-tree xs)))
    (setf (cdr (last ys)) ys)))

(defun take (n xs)
  (if (> n 0) (cons (car xs) (take (- n 1) (cdr xs)))
              nil))

(defun drop (n xs)
  (if (> n 0) (drop (- n 1) (cdr xs))
              xs))

(defun circle-repoint-start (xs old-start new-start)
  (if (eq (cdr xs) old-start) (setf (cdr xs) new-start)
                              (circle-repoint-start (cdr xs) old-start new-start)))

(defun circle-drop (n xs)
  (let ((ys (drop n xs)))
    (circle-repoint-start ys xs ys)))

(defun circle-cons (x xs)
  (let ((ys (cons x xs)))
    (circle-repoint-start (cdr ys) xs ys)))

(defun main ()
  (setq *print-circle* t)
  (let ((part1 (circle (list 9 1 6 4 3 8 2 7 5))))
    (format t "Part 1: ~S~%" part1)))
