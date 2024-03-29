;;; Problem: find the sum of all numbers that are palindromes in both base 10
;;;          base 2 from 1 to 1 000 000.
;;; (time (problem-36 1000000))

(defun palindrome-p (string)
  (equal string (reverse string)))

(defun problem-36 (num)
  (let ((sum 0)
        (i-10 0)
        (i-2 0))
    (loop for i from 1 to num do
         (setf i-10 (write-to-string i))
         (setf i-2 (write-to-string i :base 2))
         (when (and (palindrome-p i-10)
                  (palindrome-p i-2))
           (setf sum (+ sum i))))
    sum))