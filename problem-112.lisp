;;; Problem: an increasing number has no digit less than the previous 
;;;          (i.e. 12334)
;;;          a decreasing number has no digit greater than the previous
;;;          (i.e. 43321)
;;;          a bouncy number is neither increasing nor decreasing.
;;;          find the first number where 99% of the numbers below it are 
;;;          bouncy.
;;; (time (problem-112))

(labels ((digits (num) (map 'vector #'digit-char-p (princ-to-string num))))

  (defun increasing-p (num)
    (let ((digs (digits num)))
      (not
       (loop for i from 1 to (1- (length digs)) do
            (if (> (svref digs (1- i)) (svref digs i))
                (return t))))))
  
  (defun decreasing-p (num)
    (let ((digs (digits num)))
      (not
       (loop for i from 1 to (1- (length digs)) do
            (if (< (svref digs (1- i)) (svref digs i))
                (return t)))))))

(defun bouncy-p (num)
  (not (or (increasing-p num) (decreasing-p num))))

(defun print-frac (d stream depth)
  (format stream "~,0F / ~,0F" (frac-num d) (frac-den d)))

(defstruct (fraction (:conc-name frac-)
                     (:print-function print-frac))
  (num 0)
  (den 1))

(defun frac-reduce (frac)
  (if (or (zerop (frac-num frac)) (zerop (frac-den frac)))
      frac
      (let ((div (gcd (frac-num frac) (frac-den frac))))
        (make-fraction :num (/ (frac-num frac) div)
                       :den (/ (frac-den frac) div)))))
  
(defun frac-equal (a b)
  (and (equal (frac-num a) (frac-num b))
       (equal (frac-den a) (frac-den b))))

(defun problem-112 ()
  (let ((proportion (make-fraction :num 0 :den 99))
        (current 100)
        (ninety-nine (make-fraction :num 99 :den 100)))
    (loop while (not (frac-equal (frac-reduce proportion) ninety-nine)) do
         (when (bouncy-p current)
           (incf (frac-num proportion)))
         (incf (frac-den proportion))
         (incf current))
    (1- current)))
