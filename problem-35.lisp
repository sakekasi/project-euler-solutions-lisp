(defun prime-p (x)
  (not
   (loop for i from 2 to (sqrt x) do
       (if (zerop (mod x i))
           return t))))

(defun rotate (x)
  (
  