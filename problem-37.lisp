(defun digits (n) (map 'vector #'digit-char-p (princ-to-string n)))
(defun num (digits) 
  (let ((numb 0)
        (wdigits (reverse digits)))
    (loop for digit across digits do
         (incf numb digit)
         (setf numb (* numb 10)))
    (/ numb 10)))

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
      (loop for i from 0 to (1- length) do
           (when (= 1 (sbit primes i))
             collect i)))))


;(let (primes (primes-below 1000000))
;  (defun prime-p (num)
;    (if (< num (length primes))
;        (find num primes)
;        (if (< num 3)
;            (equal num 2)
;            (not 
;             (loop for i from 3 to (floor (sqrt num)) do
;                  (when (zerop (mod num i))
;                    (return t))))))))

(defun prime-p (num)
  (if (< num 3)
      (equal num 2)
      (not
       (loop for i from 3 to (floor (sqrt num)) do
            (when (zerop (mod num i))
              (return t))))))

(defun right-truncable (number)
  (labels ((work (dgts)
             (when (prime-p (num dgts))
               (if (equal 1 (length dgts))
                   t
                   (work (subseq dgts 0 (1- (length dgts))))))))
    (work (digits number))))

(defun left-truncable (number)
  (labels ((work (dgts)
               (when (prime-p (num dgts))
                 (if (equal 1 (length dgts))
                     t
                     (work (subseq dgts 1))))))
    (work (digits number))))




(defun problem-37 ()
  (let ((count 0)
        (current 8)
        (nums nil))
    (loop while (< count 11) do
         (when (and (left-truncable current) (right-truncable current))
           (print current)
           (push current nums)
           (incf count))
         (incf current))
    (reduce #'+ nums)))