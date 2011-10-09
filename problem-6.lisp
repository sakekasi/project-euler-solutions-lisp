;;; Problem: Find the difference between the sum of the squares of the first 
;;;          one hundred natural numbers and the square of the sum.
;;; (time (diff 100))

(defun sum-squares (num)
  (let ((sum 0))
    (loop for x from 1 to num do
	  (setq sum (+ sum (expt x 2))))
    sum))

(defun square-sum (num)
  (let ((sum 0))
    (loop for x from 1 to num do
	  (setq sum (+ sum x)))
    (setq sum (expt sum 2))
    sum))

(defun diff (num)
  (abs (- (sum-squares num) (square-sum num))))
