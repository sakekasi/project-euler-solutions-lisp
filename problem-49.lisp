(defun same-arith (a b c)
  (let ((x 0)
        (y 0)
        (z 0))
    (multiple-value-bind (x y z) (order a b c))
    (equal (- y x) (- z y))))

(defun order (a b c)
  (if (< a b)
      (if (< b c)
          (values a b c)
          (if (< a c)
              (values a c b)
              (values c a b)))
      (if (< a c)
          (values b a c)
          (if (< b c)
              (values b c a)
              (values c b a)))))

(defun same-digits (a b)
  (let ((adigs (sort (digits a) #'>))
        (bdigs (sort (digits b) #'>)))
    (equalp adigs bdigs)))
    
(defun digits (n) (map 'vector #'digit-char-p (princ-to-string n)))

(defun seive (num)
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
      (loop for i from 0 to (1- (length primes)) when (= 1 (sbit primes i))
             collect i))))


(defun problem-49 ()
  (let ((primes (seive 9999))
        (prev (vector 1487 4817 8147)))
    (block out
      (loop for a in primes do
           (loop for b in primes do
                (loop for c in primes do
                     (if (not (or (= a b) (= b c) (= a c)))
                         (if (and (equal (digits a) 4)
                                  (equal (digits b) 4)
                                  (equal (digits c) 4)
                                  (and (same-digits a b)
                                       (same-digits b c))
                                  (same-arith a b c)
                                  (not (equalp (order a b c) prev)))
                             (return-from out (multiple-value-list 
                                               (order a b c)))))))))))
