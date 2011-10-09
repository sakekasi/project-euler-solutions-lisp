;;; Problem: What is the 10001st prime number?
;;; (time (prime 10001))

(defun prime-p (x)
    (if (< x 3)
        (equal x 2)
        (if (zerop (mod x 2))
            nil
            (not
             (loop for i from 3 to (floor (sqrt x)) by 2 do
                  (if (zerop (mod x i))
                      (return t)))))))
              

(defun prime (num)
  (let ((cnt 0)
	(prime 2))
    (loop until (eql cnt num) do
	  (if (prime-p prime)
	      (incf cnt))
	  (incf prime))
    (decf prime)
    prime))