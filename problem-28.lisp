;;; Problem: Find the sum of the numbers in the diagonals of a 1001 by 1001 
;;;          spiral.
;;; (time (diag-sum 1001))

(defun num-levels (dim)
  (/ (+ dim 1) 2))

(defun addend (level)
  (* 2 (- level 1)))

(defun diag-sum (dim)
  (let ((num 1)
        (sum 1))
    (loop for lev from 2 to (num-levels dim) do
         (loop for i from 1 to 4 do
              (print num)
              (setf num (+ num (addend lev)))
              (setf sum (+ sum num))))
    sum))