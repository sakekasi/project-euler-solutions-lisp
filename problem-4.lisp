;;; Problem: Find the largest palindrome made from the product of two 3-digit 
;;;          numbers.
;;; (time (largest-three-digit-palindrome))

(defun is-palindrome (num)
  (let ((reverse 0)
        (n num))
    (loop while (> n 0) do	  
	  (setq reverse (+ reverse (mod n 10)))
	  (setq n (truncate n 10))
	  (setq reverse (* reverse 10)))
    (setq reverse (truncate reverse 10))
    (equal reverse num)))


(defun largest-three-digit-palindrome ()
  (let ((largest 0)
	(test 0))
    (loop for x from 100 to 999 do
	  (loop for y from x to 999 do
		(setq test (* x y))
		(if (is-palindrome test)
		    (if (> test largest)
			(setq largest test)))))
    largest))