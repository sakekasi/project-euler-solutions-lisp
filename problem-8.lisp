;;; Problem: Find the greatest product of five consecutive digits in the 1000-
;;;          digit number int problem-8.txt.
;;; (time (max-consecutive-product (string-to-list (read-number))))

(defun read-number ()
  (let ((in (open "problem-8.txt" :if-does-not-exist nil)))
    (setq line (read-line in))
    (close in))
  line)

(defun string-to-list (str)
  (let ((list nil)
	(num (parse-integer str)))
    (loop while (not (eql 0 num)) do
	  (setq list (append list (cons (mod num 10) nil)))
	  (setq num (truncate num 10)))
    list))

(defun multiply-first-five (list)
  (let ((product 0))
    (setq product (* (first list)
		     (second list)
		     (third list)
		     (fourth list)
		     (fifth list)))
    product))

(defun max-consecutive-product (list)
  (let ((largest 0)
	(product 0)
	(wlist list))
    (loop while (>= (list-length wlist) 5) do
	  (setq product (multiply-first-five wlist))
	  (if (> product largest)
	      (setq largest product))
	  (setq wlist (cdr wlist)))
    largest))