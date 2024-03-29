;;; Problem: What is the greatest product of four adjacent numbers in any 
;;;          direction (up, down, left, right, or diagonally) in the 20�20 
;;;          grid in problem-11.txt?
;;; (time (largest-product))

(defun string-to-list (s)
  (assert (stringp s) (s) "~s should be a string but is not")
  (coerce s 'list))

(defun read-lines ()
  (let ((in (open "problem-11.txt" :if-does-not-exist nil))
	(lines nil))
    (when in
      (loop for line = (read-line in nil)
	   while line do
	   (setf lines (append lines (cons line nil))))
      (close in))
    lines))

(defun tokenize (string)
  (let ((list (string-to-list string))
	(buffer nil)
	(ret nil))
    (loop for c in list do
	 (if (or (equal c '#\Space) (equal c '#\Newline))
	     (progn
	       (setf ret (append ret (cons 
				      (concatenate 'string buffer)
				      nil)))
	       (setf buffer nil))
	     (setf buffer (append buffer (cons c nil)))))
    (setf ret (append ret (cons
			   (concatenate 'string buffer)
			   nil)))
    ret))

(defun consolidate-list (list)
  (let ((vec (eval `(vector ,@list))))
    (loop for i from 0 to (- (length vec) 1) do
	 (setf (svref vec i) (parse-integer (svref vec i))))
    vec))

(defun create-array ()
  (let ((list nil)
	(lines (read-lines))
	(height 0)
	(width 0)
	(line ""))
    (loop for l in lines do
	 (setf line (consolidate-list (tokenize l)))
	 (setf width (length line))
	 (setf list (append list (cons line nil))))
    (setf height (length list))
    (let ((array (make-array `(,height ,width))))
      (loop for h from 0 to (- height 1) do
	   (loop for w from 0 to (- width 1) do
		(setf
		 (aref array h w)
		 (svref (nth h list) w))))
      array)))

(defun product (vector)
  (let ((prod 1)
	(vsize (- (length vector) 1)))
    (loop for i from 0 to vsize do
	 (setf prod (* prod (svref vector i))))
    prod))

(defun largest-horizontal-forward (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for y from 0 to (- height 1) do
	 (loop for x from 0 to (- width 4) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array y (+ x i))))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))

(defun largest-horizontal-back (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for y from 0 to (- height 1) do
	 (loop for x from 3 to (- width 1) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array y (- x i))))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))
	      

(defun largest-vertical-down (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for x from 0 to (- width 1) do
	 (loop for y from 0 to (- height 4) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array (+ y i) x)))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))

(defun largest-vertical-up (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for x from 0 to (- width 1) do
	 (loop for y from 3 to (- height 1) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array (- y i) x)))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))

(defun largest-diagonal-right-down (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for y from 0 to (- height 4) do
	 (loop for x from 0 to (- width 4) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array (+ y i) (+ x i))))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))

(defun largest-diagonal-left-down (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for y from 0 to (- height 4) do
	 (loop for x from 3 to (- width 1) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array (+ y i) (- x i))))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))

(defun largest-diagonal-right-up (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for y from 3 to (- height 1) do
	 (loop for x from 0 to (- width 4) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array (- y i) (+ x i))))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))

(defun largest-diagonal-left-up (array)
  (let ((largest 0)
	(buffer (make-array 4))
	(height (array-dimension array 0))
	(width (array-dimension array 1)))
    (loop for y from 3 to (- height 1) do
	 (loop for x from 3 to (- width 1) do
	      (loop for i from 0 to 3 do
		   (setf (svref buffer i)
			 (aref array (- y i) (- x i))))
	      (if (> (product buffer) largest)
		  (progn
		    (setf largest (product buffer))))))
    largest))

(defun largest-list (vector)
  (let ((lgst 0))
    (loop for i in vector do
	 (if (< lgst i)
	     (setf lgst i)))
    lgst))


(defun largest-product ()
  (let ((values (create-array))
	(largest nil))
    (setf largest (append largest (cons (largest-horizontal-forward values) nil)))
    (setf largest (append largest (cons (largest-horizontal-back values) nil)))
    (setf largest (append largest (cons (largest-vertical-down values) nil)))
    (setf largest (append largest (cons (largest-vertical-up values) nil)))
    (setf largest (append largest (cons (largest-diagonal-left-up values) nil)))
    (setf largest (append largest (cons (largest-diagonal-left-down values) nil)))
    (setf largest (append largest (cons (largest-diagonal-right-up values) nil)))
    (setf largest (append largest (cons (largest-diagonal-right-down values) nil)))
    (largest-list largest)))