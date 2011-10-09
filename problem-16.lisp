;;; Problem: What is the sum of the digits of the number 2^1000?
;;; (time (sum (string-to-vector (parse-string (expt 2 1000)))))

(defun parse-string (something)
  (prin1-to-string something))

(defun string-to-vector (string)
  (let ((vector (make-array 0 :fill-pointer t :adjustable t)))
    (loop for c across string do
	 (vector-push-extend (parse-integer (string c)) vector))
    vector))

(defun sum (vector)
  (let ((sum 0))
    (loop for i across vector do
	 (setf sum (+ sum i)))
    sum))