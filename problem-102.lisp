;;; Problem: find the number of triangles listed in problem-102.txt that 
;;;          contain the origin.
;;; (time (problem-102))

(ql:quickload "split-sequence")

(defstruct (point (:conc-name pt-)
                  (:print-function print-point))
  (x 0)
  (y 0))

(defun print-point (p stream depth)
  (format stream "(~A, ~A)" (pt-x p) (pt-y p)))

(defstruct (math-vector (:conc-name vec-)
                        (:print-function print-math-vector))
  (x 0)
  (y 0))

(defun print-math-vector (v stream depth)
  (format stream "<~,2F, ~,2F> " (vec-x v) (vec-y v)))

(defun math-vector (A B)
  (let* ((x (- (pt-x B) (pt-x A)))
         (y (- (pt-y B) (pt-y A))))
         (make-math-vector :x x
                           :y y)))

(defun magnitude (vector)
  (sqrt (+ (expt (vec-x vector) 2) (expt (vec-y vector) 2))))

(defun mvector+ (A B)
  (make-vector :x (+ (vec-x A) (vec-x B))
               :y (+ (vec-y A) (vec-y B))))

(defun mvector* (n A)
  (make-vector :x (* n (vec-x A))
               :y (* n (vec-y A))))

(defun mvector. (A B)
  (+ (* (vec-x A) (vec-x B)) (* (vec-y A) (vec-y B))))

(defstruct (triangle (:conc-name tri-))
  a
  b
  c)

(defun inside (tri pt)
  (let* 
      ; vectors
      ((A-B (math-vector (tri-a tri)
                         (tri-b tri)))
       (A-C (math-vector (tri-a tri)
                         (tri-c tri)))
       (A-P (math-vector (tri-a tri)
                         pt))
       ;dot products
       (C.C (mvector. A-C A-C))
       (C.B (mvector. A-C A-B))
       (C.P (mvector. A-C A-P))
       (B.B (mvector. A-B A-B))
       (B.P (mvector. A-B A-P))

       ;barycentrics
       (u (/ (- (* B.B C.P) (* C.B B.P))
             (- (* C.C B.B) (* C.B C.B))))
       (v (/ (- (* C.C B.P) (* C.B C.P))
             (- (* C.C B.B) (* C.B C.B)))))
    (and (> u 0) (> v 0) (> 1 (+ u v)))))

(defun problem-102 ()
  (let ((file-stream (open "problem-102.txt"))
        (line nil)
        (coords nil)
        (count 0)
        (tri nil)
        (origin (make-point)))
    (loop 
       while (not (eql (setf line (read-line file-stream nil 'eof)) 'eof)) do
         (setf coords (split-sequence:split-sequence #\, line))
         (setf tri (make-triangle :a (make-point :x (parse-integer 
                                                     (nth 0 coords))
                                                 :y (parse-integer
                                                     (nth 1 coords)))
                                  :b (make-point :x (parse-integer
                                                     (nth 2 coords))
                                                 :y (parse-integer
                                                     (nth 3 coords)))
                                  :c (make-point :x (parse-integer 
                                                     (nth 4 coords))
                                                 :y (parse-integer
                                                     (nth 5 coords)))))
         (when (inside tri origin)
           (incf count)))
    (close file-stream)
    count))