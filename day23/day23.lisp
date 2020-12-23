(defun circle (xs)
  (let ((ys (copy-tree xs)))
    (setf (cdr (last ys)) ys)))

(defun take (n xs)
  (if (> n 0) (cons (car xs) (take (- n 1) (cdr xs)))
              nil))

(defun drop (n xs)
  (if (> n 0) (drop (- n 1) (cdr xs))
              xs))

(defun circle-unroll-impl (xs start)
  (if (eq (cdr xs) start) (cons (car xs) nil)
                          (cons (car xs) (circle-unroll-impl (cdr xs) start))))

(defun circle-unroll (xs)
  (circle-unroll-impl xs xs))

(defun circle-repoint-start (xs old-start new-start)
  (if (eq (cdr xs) old-start) (setf (cdr xs) new-start)
                              (circle-repoint-start (cdr xs) old-start new-start)))

(defun circle-drop (n xs)
  (let ((ys (drop n xs)))
    (circle-repoint-start ys xs ys)))

(defun circle-cons (x xs)
  (let ((ys (cons x xs)))
    (circle-repoint-start (cdr ys) xs ys)))

(defun circle-append (xs ys)
  (if xs (circle-cons (car xs) (circle-append (cdr xs) ys))
         ys))

(defun circle-find-impl (f xs start)
  (cond ((funcall f (car xs)) xs)
        ((eq (cdr xs) start) nil)
        (t (circle-find-impl f (cdr xs) start))))

(defun circle-find (f xs)
  (circle-find-impl f xs xs))

(defun iterate (n f x)
  (format t "~S~S~%" (take 10 x) (take 30 (drop 999970 x)))
  (if (> n 0) (iterate (- n 1) f (funcall f x))
              x))

(defun range (n m)
  (if (< n m) (cons n (range (+ n 1) m))
              nil))

(defun dest-cup (x mn mx xs)
  (if (< x mn) (dest-cup mx mn mx xs)
               (let ((zs (circle-find (lambda (y) (eql x y)) xs)))
                  (if zs zs
                         (dest-cup (- x 1) mn mx xs)))))

(defun move (xs)
  (let ((x (car xs))
        (ts (take 3 (cdr xs))))
    (circle-drop 3 (cdr xs))
    (let* ((xsl (cdr (circle-unroll xs)))
           (zs (dest-cup (- x 1) (apply 'min xsl) (apply 'max xsl) xs)))
      (circle-append ts (cdr zs))
      (cdr xs))))

(defun main ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (setq *print-circle* t)
  (let* ((inputl1 '(9 1 6 4 3 8 2 7 5))
         (input1 (circle inputl1))
         (final1 (cdr (circle-find (lambda (x) (eql x 1)) (iterate 100 'move input1)))))
    (format t "Part 1: ~S~%" final1)
    (let* ((n (length inputl1))
           (mx (apply 'max inputl1))
           (inputl2 (append inputl1 (range (+ mx 1) (+ (+ mx 1) (- 1000000 n)))))
           (input2 (circle inputl2))
           (final2 (cdr (circle-find (lambda (x) (eql x 1)) (iterate 10000000 'move input2))))
           (part2 (* (car final2) (car (cdr final2)))))
      (format t "Part 2: ~S~%" part2))))
