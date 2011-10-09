;;; Problem: find the denominator of the product of the four double digited
;;;          fractions that are correctly reduced in both the right way and
;;;          in the manner (49/98 = 4/8).
;;; (time (problem-33))

(defstruct fraction
  (num 0)
  (den 1))

(defun fraction-reduce (frac)
  (let ((gcf (gcd (fraction-num frac) 
                  (fraction-den frac))))
    (make-fraction :num (/ (fraction-num frac) gcf)
                   :den (/ (fraction-den frac) gcf))))

(defun digits (num)
  (let ((dig (make-array 0 :adjustable t :fill-pointer t))
        (wnum num))
    (loop while (> wnum 0) do
         (vector-push-extend (mod wnum 10) dig)
         (setf wnum (truncate wnum 10)))
    (reverse dig)))

(defun vector-number (vec)
  (let ((wrk (reverse vec))
        (num 0))
    (loop for i from 0 to (- (length vec) 1) do
         (setf num (+ num (* (expt 10 i) (aref wrk i)))))
    num))

(defun vector-intersection (vec1 vec2)
  (let ((list1 (coerce vec1 'list))
        (list2 (coerce vec2 'list)))
    (intersection list1 list2)))

(defun common-digit (a b)
  (let ((a-digs (digits a))
        (b-digs (digits b)))
    (vector-intersection a-digs b-digs)))

(defun fraction-silly-reduce (frac)
  (let* ((reducable (common-digit (fraction-num frac)
                                 (fraction-den frac)))
        (trivial (zerop (if reducable
                            (car reducable)
                            1)))
        (num-digs (digits (fraction-num frac)))
        (den-digs (digits (fraction-den frac)))
        (frac (make-fraction)))
    (when reducable
      (setf frac (make-fraction :num (vector-number
                           (remove (car reducable) num-digs))
                     :den (vector-number
                           (remove (car reducable) den-digs)))))
    (values frac (car reducable) trivial)))
      

(defun fraction-print (frac)
  (print (fraction-num frac))
  (print (fraction-den frac))
  (format nil "~a/~a" (fraction-num frac) (fraction-den frac)))

(defun fraction-multiply (&rest args)
  (let ((product (make-fraction :num 1)))
    (loop for i in args do
         (setf (fraction-num product) 
               (* (fraction-num i) (fraction-num product)))
         (setf (fraction-den product)
               (* (fraction-den i) (fraction-den product))))
    product))

(defun fraction-equal (x y)
  (if (or (zerop (fraction-num x))
          (zerop (fraction-num y)))
      (equal (fraction-num x)
             (fraction-num y))
      (let ((a (fraction-reduce x))
            (b (fraction-reduce y)))
        (and (equal (fraction-num a)
                    (fraction-num b))
             (equal (fraction-den a)
                    (fraction-den b))))))

(defun problem-33 ()
  (let ((product (make-fraction :num 1))
        (wfrac (make-fraction)))
    (loop for num from 10 to 99 do
         (loop for den from (+ num 1) to 99 do
              (setf wfrac (make-fraction :num num :den den))
              (when (and
                     (fraction-equal (fraction-reduce wfrac)
                                    (fraction-silly-reduce wfrac))
                     (not (third (multiple-value-list
                                  (fraction-silly-reduce wfrac)))))
                (setf product (fraction-multiply product wfrac)))))
    (fraction-den (fraction-reduce product))))

