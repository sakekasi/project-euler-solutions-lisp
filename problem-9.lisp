;;; Problem: Find the product abc of the pythagorean triplet (a, b, c) where
;;;          a + b + c = 1000
;;; (time (pyth-prod))

(defun is-pyth-triple (a b c)
  (eql (expt c 2)
       (+ (expt a 2) (expt b 2))))

(defun pyth-prod ()
  (let ((prod 0))
    (loop for a from 1 to 1000 do
	  (loop for b from a to 1000 do
		(setq c (- 1000 (+ a b)))
		(if (is-pyth-triple a b c)
		    (setq prod (* a b c)))))
    prod))