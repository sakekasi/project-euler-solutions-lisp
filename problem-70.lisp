(defun digits (n) (map 'vector #'digit-char-p (princ-to-string n)))

(defun coprime-p (a n)
  (equal 1 (gcd a n)))

(defun totient (n)
  (let ((count 0))
    (loop for i from 1 to n do
         (when (coprime-p i n)
           (incf count)))
    count))

(defun permutation-p (a b)
  (equalp (sort (digits a) #'>)
          (sort (digits b) #'>)))

(defun problem-70 ()
  (let ((ceil (expt 10 7))
        (nums (make-array 0 :adjustable t :fill-pointer t))
        (ratios (make-array 0 :adjustable t :fill-pointer t))
        (tot 0)
        (min 0))
    (loop for i from 2 to ceil do
         (setf tot (totient i))
         (when (permutation-p i tot)
           (print i)
           (vector-push-extend i nums)
           (vector-push-extend (float (/ i tot)) ratios)))
    (loop for i from 0 to (1- (length nums)) do
         (when (< (aref ratios i) (aref ratios min))
           (setf min i)))
    (aref nums min)))
           
         