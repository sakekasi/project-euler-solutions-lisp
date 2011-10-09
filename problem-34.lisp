;;; Problem: find the sum of all factorions that are not 1 or 2.
;;; (time (problem-34))


(defun digits (num)
  (let ((digs ())
        (wnum num))
    (loop while (> wnum 0) do
         (push (mod wnum 10) digs)
         (setf wnum (truncate wnum 10)))
    digs))

(defun factorial (num)
  (if (zerop num)
      1
      (* num (factorial (- num 1)))))

(defun factorion-p (num)
  (let ((digs (digits num))
        (sum-fact 0))
    (loop for i in digs do
         (setf sum-fact (+ sum-fact (factorial i))))
    (equal sum-fact num)))

(defun problem-34 ()
  (let ((sum 0))
    (loop for i from 3 to 2540160 do
         (when (factorion-p i)
           (setf sum (+ sum i))))
    sum))