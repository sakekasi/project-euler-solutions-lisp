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


(defun abundant-p (num)
  (let ((d (lambda (x) (reduce #'+ (proper-divisors x)))))
    (if (> (funcall d num) num)
        t
        nil)))

(defun deficient-p (num)
  (let ((d (lambda (x) (reduce #'+ (proper-divisors x)))))
    (if (< (funcall d num) num)
        t
        nil)))

(defun perfect-p (num)
  (and (not (abundant-p num)) (not (deficient-p num))))

(defun even-abundants (num)
  (let ((evens (make-array 0 :fill-pointer t :adjustable t)))
    (loop for i from 12 to num by 2 do
         (when (abundant-p i)
           (vector-push-extend i evens)))
    (sort evens #'<)))

(defun odd-abundants (num)
  (let ((odds (make-array 0 :fill-pointer t :adjustable t)))
    (loop for i from 945 to num by 2 do
         (when (abundant-p i)
           (vector-push-extend i odds)))
    (sort odds #'<)))

(defun abundant-sum-odd-p (num odds evens)
  (let ((ret nil)
        (i 0)
        (j 0)
        (a 0)
        (b 0))
    (loop while (and
                 (< i (length odds))
                 (<= num (setf a (aref odds i)))) do
         (if (not ret)
             (progn
               (setf j 0)
               (loop while (and
                            (< j (length evens))
                            (<= (- num a) (setf b (aref evens j)))) do
                    (when (equal num (+ a b))
                      (setf ret t)
                      (return t))
                    (incf j)))
             (return t))
         (incf i))
    ret))

(defun sum-not-abundant ()
  (let ((sum 266)
        (odds (odd-abundants 20161))
        (evens (even-abundants 20161)))
    (loop for i from 1 to 20161 by 2 do
         (when (not (abundant-sum-odd-p i odds evens))
           (print i)
           (setf sum (+ sum i))))
    sum))