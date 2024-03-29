;;; Problem: What is the smallest positive number that is evenly divisible by 
;;;          all of the numbers from 1 to 20?
;;; (time (smallest-product (list-from 1 20)))

(defun is-prime (x)
    (if (< x 3)
        (eql x 2)
  	(let ((count 2)
              (isp t))
	     (loop (when (> count (sqrt x)) (return isp))
	         (if (eql 0 (mod x count))
                     (setq isp nil))
		     (incf count)))))

(defun primes-until (num)
  (let ((primes nil))
    (loop for x from 2 to num do
	  (if (is-prime x)
	      (setq primes (append primes (cons x nil)))))
    primes))

(defun multiply-all (list)
  (let ((prod 1))
    (loop for x in list do
	  (setq prod (* prod x)))
    prod))

(defun list-from (low high)
  (let ((list nil))
    (loop for x from low to high do
	  (setq list (append list (cons x nil))))
    list))

(defun has-all-factors (num list)
  (let ((test t))
    (loop for x in list do
	  (if (not (eql (mod num x) 0))
	      (setq test nil)))
    test))

(defun maximum (list)
  (let ((largest 0))
    (loop for x in list do
	 (if (> x largest)
	     (setq largest x)))
    largest))

(defun smallest-product (list)
  (let ((test (multiply-all (primes-until (maximum list))))
	(increment (multiply-all (primes-until (maximum list)))))
    (loop while (not (has-all-factors test list)) do
	  (setq test (+ test increment)))
    test))	