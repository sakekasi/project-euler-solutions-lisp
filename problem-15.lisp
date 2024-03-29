;;; Problem: find the number of unique routes to go from the top left corner
;;;          to the bottom right corner of a 20 x 20 grid without 
;;;          backtracking.
;;; (time (number-paths 20))

(defun factorial (n)
  (if (zerop n)
      1
      (* n (factorial (- n 1)))))

(defun pascal (row number)
  (/ (factorial row) (* (factorial number) (factorial (- row number)))))

(defun number-paths (dimension)
  (let* ((num-junc (+ dimension 1))
	 (row-num (- (* num-junc 2) 2))
	 (num (- (/ (+ row-num 2) 2) 1)))
    (pascal row-num num)))    