;;; Problem: Which starting number under a million produces the longest
;;;          collatz sequence?
;;; (time (largest-collatz-under))
;;;
;;; NOTE: Runs quickly but puts heavy load on slime. Run in clisp outside
;;;       EMACS.

(defparameter *collatz-hash* (make-hash-table :size 1500000 :rehash-size 10))


(defun collatz (element)
  (if (equal element 1)
      (vector 1)
      (if (gethash element *collatz-hash*)
	  (gethash element *collatz-hash*)
	  (if (zerop (mod element 2))
	      (setf (gethash element *collatz-hash*) 
		    (concatenate 'vector (vector element) (collatz (/ element 2))))
	      (setf (gethash element *collatz-hash*)
		    (concatenate 'vector (vector element) (collatz 
							   (+ (* 3 element) 1))))))))

(defun largest-collatz-under ()
  (let ((largest-number 0)
	(largest 0)
	(len 0))
    (loop for i from 1 to 999999 by 2 do
	 (setf len (length (collatz i)))
	 (when (> len largest)
	   (print i)
	   (setf largest len)
	   (setf largest-number i)))
    largest-number))

