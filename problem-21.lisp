;;; Problem: find the sum of all amicable pairs less than 10000
;;; (time (amicable-sum))

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
		rest
		:initial-value (apply fn1 args)))))

(defun proper-divisors (num)
  (let ((flsq (floor (sqrt num)))
	(sq (sqrt num))
	(divisors (make-array 0 :fill-pointer t :adjustable t)))
    (loop for i from 2 to flsq do
	 (if (zerop (mod num i))
	     (progn
	       (vector-push-extend i divisors)
	       (if (not (equal sq i))
		   (vector-push-extend (/ num i) divisors)))))
    (vector-push-extend 1 divisors)
    (sort divisors #'>)))

(defun sum (vec)
  (let ((sum 0))
    (loop for i across vec do
	 (setf sum (+ sum i)))
    sum))

(defun amicablep (x y)
  (let ((divx (sum (proper-divisors x)))
	(divy (sum (proper-divisors y))))
    (and (= divy x) (= divx y))))

(defun amicable-sum ()
  (let ((sum 0)
	(d (compose #'sum #'proper-divisors))
	(b 0))
    (loop for a from 1 to 10000 do
	 (setf b (funcall d a))
	 (when (and (equal (funcall d b) a) (not (equal b a)))
	   (setf sum (+ sum a b))))
    (/ sum 2)))