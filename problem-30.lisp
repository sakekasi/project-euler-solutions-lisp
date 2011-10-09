;;; Problem: find the sum of all numbers that can be written as the sum
;;;          of the fifth powers of all their digits.
;;; (time (problem-30))

(defun digits (n) (map 'vector #'digit-char-p (princ-to-string n)))

(defun sum-fifth-p (n)
  (equal n (reduce #'+ (map 'vector (lambda (n) (expt n 5)) (digits n)))))

(defun problem-30 ()
  (let ((sum 0))
    (loop for i from 2 to 295245 do
         (if (sum-fifth-p i)
             (incf sum i)))
    sum))