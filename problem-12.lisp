;;; Problem: What is the value of the first triangle number to have over
;;;          five hundred divisors?
;;; (time (triangle-five-hundred))
;;; SPEED ME UP

(defun num-divisors (num)
  (let ((flsq (floor (sqrt num)))
        (count 0))
    (loop for i from 1 to flsq do
         (when (zerop (mod num i))
           (if (equal i flsq)
               (incf count)
               (incf count 2))))
    count))

(defun triangle-five-hundred ()
  (let ((tnum 1)
	(current 2)
	(ndivisors 1))
    (loop while (<= ndivisors 500) do
	 (setf tnum (+ tnum current))
	 (setf ndivisors (num-divisors tnum))
	 (incf current))
    tnum))