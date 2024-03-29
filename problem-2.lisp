;;; Problem: Find the sum of the fibonacci numbers that are even and less
;;;          than 4 000 000.
;;; (time (fib))

(defun fib ()
  (let ((a 1)
        (b 2)
        (temp 0)
        (sum 2))
    (loop (when (> b 3999999) (return sum))
      (setq temp a
            a b
            b (+ a temp))
      (if (eql 0 (mod b 2))
          (setq sum (+ sum b))))))

