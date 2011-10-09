;(defun pentagonal-p (num)
;  (zerop (mod (/ (1+ (sqrt (1+ (* 24 num)))) 6) 1)))

;(defun hexagonal-p (num)
;  (zerop (mod (/ (1+ (sqrt (1+ (* 8 num)))) 4) 1)))

;(defun triangular-p (num)
;  (zerop (mod (/ (1- (sqrt (1+ (* 8 num)))) 2) 1)))

;(defun hexagonal (n)
;  (* n (1- (* 2 n))))


;(defun problem-45 ()
;  (let ((hex 0))
;    (loop for n from 144 to 1000000000 do
;         (setf hex (hexagonal n))
;         (when (and (triangular-p hex)
;                    (pentagonal-p hex))
;           (return hex)))))

(defun euler45 (&optional (h 144))
  (let* ((hn (* h (- (* 2 h) 1)))
         (tn (/ (+ -1 (sqrt (+ 1 (* 8 hn)))) 2))
         (pn (/ (+ 1 (sqrt (+ 1 (* 24 hn)))) 6)))
    (print hn)
    (if (and (integerp tn) (integerp pn))
        hn
        (euler45 (+ h 1)))))