;;; Problem: Work out the first ten digits of the sum of the one-hundred 
;;;          50-digit numbers in problem-13.txt
;;; (time (sum-array (read-to-array)))

(defun read-to-array ()
  (let ((nums (make-array '(100 50)))
	(lino 0))
    (with-open-file (in "problem-13.txt")
      (do ((line (read-line in nil)
		 (read-line in nil)))
	  ((null line))
	(let ((charno 0))
	  (loop for char across line do
	       (setf (aref nums lino charno)
		     (parse-integer (string char)))
	       (incf charno)))
	(incf lino)))
    nums))

(defun num-to-vector (num)
  (let ((number
	 (make-array 0 :initial-element 0 :fill-pointer t :adjustable t))
	(wnum num))
    (loop while (> wnum 0) do
	 (vector-push-extend (mod wnum 10) number)
	 (setf wnum (truncate wnum 10)))
    number))

(defun vector-to-num (vec)
  (let ((number 0)
	(vector vec))
    (loop for i from 0 to (- (length vector) 1) do
	 (setf number (+ number (* (aref vector i) (expt 10 i)))))
    number))

(defun sum-array (array)
  (let ((number 
	 (make-array 1 :initial-element 0 :fill-pointer t :adjustable t))
	(indcur 0))
    (loop for x downfrom 49 to 0 do
	 (let ((sum 0)
	       (vecsum #()))
	   (loop for y from 0 to 99 do
		(setf sum (+ sum (aref array y x))))
	   (setf vecsum (num-to-vector sum))
	   (setf vecsum (num-to-vector (vector-to-num vecsum)))
	   (loop for i from indcur to (- (length number) 1) do
		(setf (aref vecsum (- i indcur))
		      (+ (aref vecsum (- i indcur))
			 (aref number i))))
	   (setf vecsum (num-to-vector (vector-to-num vecsum)))
	   (loop for i from 0 to (- (length vecsum) 1) do
		(if (< (length number) (+ i indcur 1))
		    (vector-push-extend (aref vecsum i) number)
		    (setf (aref number (+ i indcur)) (aref vecsum i)))))
	 (incf indcur))
    (setf number (reverse number))))
;;    (loop for i from 0 to 9 do
;;	 (print (aref number i)))))
	   
	   