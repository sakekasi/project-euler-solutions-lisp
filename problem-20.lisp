;;; Problem: Find the sum of the digits in the number 100!
;;; (time (sum-vec (num-to-vec (factorial 100))))

(defun factorial (n)
  (if (zerop n)
      1
      (* n (factorial (- n 1)))))

(defun num-to-vec (number)
  (let ((string (prin1-to-string number))
	(vec (make-array 0 :fill-pointer t :adjustable t)))
    (loop for i across string do
	 (vector-push-extend (parse-integer (string i)) vec))
    vec))

(defun sum-vec (vec)
  (let ((sum 0))
    (loop for i across vec do
	 (setf sum (+ sum i)))
    sum))