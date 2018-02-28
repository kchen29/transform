(defmacro draw-line-base (x0 y0 x1 y1 plot-1 plot-2)
  "Base code for octant 1. Other octants can be gotten from transformations."
  `(do* ((x ,x0 (1+ x))
         (y ,y0)
         (A (- ,y1 ,y0))
         (B (- ,x0 ,x1))
         (2A (* 2 A))
         (2B (* 2 B))
         (d (+ 2A B) (+ d 2A)))
        ((> x ,x1))
     (plot ,plot-1 ,plot-2 screen color)
     (when (> d 0)
       (incf y)
       (incf d 2B))))

(defun draw-line (x0 y0 x1 y1 screen color)
  "Draws a line from (x0, y0) to (x1, y1) on SCREEN using COLOR."
  (when (minusp (- x1 x0))
    (rotatef x0 x1)
    (rotatef y0 y1))
  (let ((xdif (- x1 x0))
        (ydif (- y1 y0)))
    (if (>= ydif 0)
        (if (minusp (- ydif xdif))
            (draw-line-base x0 y0 x1 y1 x y)
            (draw-line-base y0 x0 y1 x1 y x))
        (if (minusp (+ ydif xdif))
            (draw-line-base y0 x0 (- y0 ydif) x1 y (- (* 2 y0) x))
            (draw-line-base x0 y0 x1 (- y0 ydif) x (- (* 2 y0) y))))))

(defun draw-lines (matrix screen color)
  "Draws the lines from MATRIX onto SCREEN with COLOR."
  (do ((index 0 (+ 2 index)))
      ((>= index (array-dimension matrix 1)))
    (draw-line (aref matrix 0 index)
               (aref matrix 1 index)
               (aref matrix 0 (1+ index))
               (aref matrix 1 (1+ index))
               screen color)))

(defun add-edge (matrix x0 y0 z0 x1 y1 z1)
  "Adds a line from point (x0 y0 z0) to (x1 y1 z1)."
  (add-point matrix x0 y0 z0)
  (add-point matrix x1 y1 z1))

(defun add-point (matrix x y &optional (z 0))
  "Adds a point (x y z) onto MATRIX.
   Appends the point as a column"
  (adjust-array matrix (list (array-dimension matrix 0)
                             (1+ (array-dimension matrix 1))))
  (let ((end (1- (array-dimension matrix 1))))
    (setf (aref matrix 0 end) x
          (aref matrix 1 end) y
          (aref matrix 2 end) z
          (aref matrix 3 end) 1)))
  
