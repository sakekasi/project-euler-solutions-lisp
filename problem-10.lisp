;;; Problem: Calculate the sum of prime numbers below 2 million
;;; (time (reduce #'+ (primes-below 2000000)))


(defun primes-below (num)
  (let ((primes (make-array (1+ num) :initial-element 1 :element-type 'bit))
        (end (floor (sqrt num))))
    (labels ((mark-multiples (num lst)
                            (let ((len (length lst)))
                              (loop for i from (expt num 2) to (1- len)
                                   by num do
                                   (when (= 1 (sbit lst i))
                                     (setf (sbit lst i) 0))))))
      (setf (sbit primes 0) 0)
      (setf (sbit primes 1) 0)
      (loop for i from 2 to end do
           (when (= 1 (sbit primes i))
             (mark-multiples i primes)))
      (loop for i from 2 to num
         when (= 1 (sbit primes i))
         collect i))))

