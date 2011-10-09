;;; Problem: find the last 10 digits of the number 1^1 + 2^2 ... 1000^1000
;;; (time (problem-48))

(defun problem-48 ()
  (let ((sum 0))
    (loop for i from 1 to 1000 do
         (incf sum (expt i i)))
    (mod sum (expt 10 10))))
