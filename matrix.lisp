(defun print-matrix (matrix)
  "Prints out MATRIX to *standard-output*."
  (format t "~{~%~{~a~4,4T~}~}~%" (matrix-to-list matrix)))

(defun matrix-to-list (matrix)
  "Turns MATRIX into a list."
  (loop for x below (array-dimension matrix 0)
     collect (loop for y below (array-dimension matrix 1)
                collect (aref matrix x y))))
                        
(defun to-identity (matrix)
  "Turns MATRIX into an identity matrix."
  (dotimes (x (array-dimension matrix 0))
    (dotimes (y (array-dimension matrix 1))
      (if (= x y)
          (setf (aref matrix x y) 1)
          (setf (aref matrix x y) 0)))))

(defun matrix-multiply (m1 m2)
  "A general matrix multiplication routine.
   Multiplies M1 with M2. Modifies M2 to hold the result."
  (let ((m3 (make-matrix (array-dimension m1 0) (array-dimension m2 1))))
    (dotimes (row (array-dimension m1 0))
      (dotimes (col (array-dimension m2 1))
        (setf (aref m3 row col) (dot row col m1 m2))))
    (copy-matrix m3 m2)))

(defun copy-matrix (m1 m2)
  "Copies the values of m1 to m2. Returns m2"
  (dotimes (x (array-dimension m1 0) m2)
    (dotimes (y (array-dimension m1 1))
      (setf (aref m2 x y) (aref m1 x y)))))

(defun dot (row col m1 m2)
  "Dots the ROW of M1 with the COL of M2. 
   They should have the same corresponding sizes."
  (loop for i below (array-dimension m1 1)
       sum (* (aref m1 row i) (aref m2 i col))))

(defun make-matrix (&optional (rows 4) (cols 4))
  "Makes a matrix with ROWS and COLS"
  (make-array (list rows cols) :adjustable t))

(defun clear-matrix (matrix)
  "Adjusts size to zero."
  (adjust-array matrix '(4 0)))
