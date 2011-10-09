;;; Problem: Find the sum of all the multiples of 3 or 5 below 1000.
;;; (time (natural-below 1000))

(defun natural-below (x)
  (let ((i 0) (sum 0))
     (loop (when (> i (- x 1) ) (return sum))
        (if (or
   	      (equal 0 (mod i 3))
	      (equal 0 (mod i 5)))
          (setq sum (+ sum i)))
        (incf i))))
