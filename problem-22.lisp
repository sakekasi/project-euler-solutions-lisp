;;; Problem: The name score of a name is its number in an alphabetically
;;;          sorted list times the numerical sume of its letters. What is
;;;          the sum of the name scores of the sorted list of the names
;;;          from problem-22.txt?
;;; (time (total-value (list-names)))

(defun letter-number (letter)
  (if (upper-case-p letter)
      (+ (- (char-code letter) (char-code #\A)) 1)
      (+ (- (char-code letter) (char-code #\a)) 1)))

(defun alphabetical-value (name)
  (let ((value 0))
    (loop for c across name do
	 (setf value (+ value (letter-number c))))
    value)
)

(defun list-names ()
  (let ((names (make-array 0 :fill-pointer t :adjustable t))
	(buffer nil))
    (with-open-file (stream "problem-22.txt")
      (do ((char (read-char stream nil)
		 (read-char stream nil)))
	  ((null char))
	(unless (char= char #\")
	  (if (char= char #\,)
	      (progn
		(vector-push-extend (concatenate 'string (reverse buffer))
				    names)
		(setf buffer nil))
	      (push char buffer))))
      (progn
	(vector-push-extend (concatenate 'string (reverse buffer)) names)
	(setf buffer nil)))
    (sort names #'string<)))

(defun total-value (names)
  (let ((sum 0)
	(name ""))
    (loop for i from 0 to (- (length names) 1) do
	 (setf name (aref names i))
	 (setf sum (+ sum (* (+ i 1) (alphabetical-value name)))))
    sum))