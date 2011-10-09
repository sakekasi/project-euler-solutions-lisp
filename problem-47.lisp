(defun prime-p (num)
  (not
   (loop for i from 2 to (floor (sqrt num)) do
        (when (zerop (mod num i))
          (return t)))))

(defun prime-factors (num)
  