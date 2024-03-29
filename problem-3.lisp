;;; Problem: What is the largest prime factor of the number 600851475143 ?
;;; (time (greatest-prime-factor 600851475143))

(defun is-prime (x)
    (if (< x 3)
        (eql x 2)
  	(let ((count 2)
              (isp t))
	     (loop (when (> count (sqrt x)) (return isp))
	         (if (eql 0 (mod x count))
                     (setq isp nil))
		     (incf count)))))
(defun next-prime (start)
  (let ((prime (+ start 1)))
    (loop while (not (is-prime prime)) do
	  (incf prime))
    prime))

(defun greatest-prime-factor-recur (num test)
  (if (eql 0 (mod num test))
      (if (is-prime (/ num test))
	  (/ num test)
	(greatest-prime-factor-recur (/ num test) test))
    (greatest-prime-factor-recur num (next-prime test))))

(defun greatest-prime-factor (num)
  (greatest-prime-factor-recur num 2))
