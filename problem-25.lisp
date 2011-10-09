;;; Problem: find the number (2 = 3, 3 = 4, 5 = 5, 8 = 6, etc.) of the first
;;;          fibonacci number with 1000 digits.
;;; (time (problem-25))

(defun num-digits (number)
  (let ((digits 0)
        (wnum number))
    (loop while (> wnum 0) do
         (incf digits)
         (setf wnum (truncate wnum 10)))
    digits))

(defun problem-25 ()
  (let ((digits 0)
        (a 1)
        (b 1)
        (temp nil)
        (count 2))
    (loop while (< digits 1000) do
         (setf temp a)
         (setf a b)
         (setf b (+ temp a))
         (setf digits (num-digits b))
         (incf count))
    count))