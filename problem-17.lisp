;;; Problem: If all the numbers from 1 to 1000 (one thousand) inclusive 
;;;          were written out in words, how many letters would be used?
;;;
;;; NOTE: Do not count spaces or hyphens and count the letters based on 
;;;       British usage.
;;;
;;; (time (letters-to-1000))

(defmacro puthash (key val ht)
  (list 'setf (list 'gethash key ht) val))

(defparameter *num-letters* (make-hash-table))
(puthash 0 0 *num-letters*)
(puthash 1 3 *num-letters*)
(puthash 2 3 *num-letters*)
(puthash 3 5 *num-letters*)
(puthash 4 4 *num-letters*)
(puthash 5 4 *num-letters*)
(puthash 6 3 *num-letters*)
(puthash 7 5 *num-letters*)
(puthash 8 5 *num-letters*)
(puthash 9 4 *num-letters*)
(puthash 10 3 *num-letters*)
(puthash 11 6 *num-letters*)
(puthash 12 6 *num-letters*)
(puthash 13 8 *num-letters*)
(puthash 14 8 *num-letters*)
(puthash 15 7 *num-letters*)
(puthash 16 7 *num-letters*)
(puthash 17 9 *num-letters*)
(puthash 18 8 *num-letters*)
(puthash 19 8 *num-letters*)
(puthash 20 6 *num-letters*)
(puthash 30 6 *num-letters*)
(puthash 40 5 *num-letters*)
(puthash 50 5 *num-letters*)
(puthash 60 5 *num-letters*)
(puthash 70 7 *num-letters*)
(puthash 80 6 *num-letters*)
(puthash 90 6 *num-letters*)
(puthash 'hundred 7 *num-letters*)
(puthash 'thousand 8 *num-letters*)
(puthash 'and 3 *num-letters*)


(defun num-to-vector (number)
  (let ((vector (make-array 0 :fill-pointer t :adjustable t))
	(working number))
    (loop while (> working 0) do
	 (vector-push-extend (mod working 10) vector)
	 (setf working (truncate working 10)))
    (reverse vector)))


(defun num-letters-tens (vec)
  (let ((number (if (equal (length vec) 1)
		    (aref vec 0)
		    (+ (* 10 (aref vec 0)) (aref vec 1)))))
    (if (< number 20)
	(gethash number *num-letters*)
	(+ (gethash (* 10 (aref vec 0)) *num-letters*) 
	   (gethash (aref vec 1) *num-letters*)))))

(defun num-letters-hundreds (vec)
  (let ((number (cond ((equal (length vec) 3)
			 (+ (* 100 (aref vec 0)) (* 10 (aref vec 1))
			    (aref vec 2)))
			((equal (length vec) 2)
			 (+ (* 10 (aref vec 0)) (aref vec 1)))
			(t (aref vec 0)))))
      (if (< (length vec) 3)
	  (num-letters-tens vec)
	  (if (zerop (mod number 100))
	      (+ (num-letters-tens (vector (aref vec 0))) 
		 (gethash 'hundred *num-letters*))
	      (+ (num-letters-tens (vector (aref vec 0)))
		 (gethash 'hundred *num-letters*)
		 (gethash 'and *num-letters*)		 
		 (num-letters-tens (vector (aref vec 1) (aref vec 2))))))))
		      
(defun num-lettrs (number)
  (if (equal number 1000)
      11
      (num-letters-hundreds (num-to-vector number))))

(defun letters-to-1000 ()
  (let ((sum 0))
    (loop for i from 1 to 1000 do
	 (setf sum (+ sum (num-lettrs i))))
    sum))