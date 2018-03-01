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

;;;transformations
(defmacro deftransform (transform-name args &body body)
  "Defuns make-transform given TRANSFORM-NAME, using args and the body.
   Also defuns do-transform, applying make-transform to another matrix."
  `(defun ,(intern (concatenate 'string "MAKE-" (symbol-name transform-name))) ,args
     ,(when (stringp (first body))
            (pop body))
     (let ((transform (make-matrix)))
       (to-identity transform)
       ,@body
       transform))
  `(defun ,(intern (concatenate 'string "DO-" (symbol-name transform-name))) ,(append args '(transform-matrix))
     ,(concatenate 'string "Applies make-" (string-downcase (symbol-name transform-name)) " to TRANSFORM-MATRIX")
     (matrix-multiply (,(intern (concatenate 'string "MAKE-" (symbol-name transform-name))) ,@args) transform-matrix)))

(deftransform translate (delx dely delz)
  "Makes a matrix that translates by DELX, DELY, and DELZ"
  (setf (aref transform 0 3) delx
        (aref transform 1 3) dely
        (aref transform 2 3) delz))

(deftransform scale (x-scale y-scale z-scale)
  "Makes a matrix that scales x by X-SCALE, y by Y-SCALE, and z by Z-SCALE"
  (setf (aref transform 0 0) x-scale
        (aref transform 1 1) y-scale
        (aref transform 2 2) z-scale))

(deftransform rotate-z (degrees)
  "Makes a matrix that rotates by DEGREES counter-clockwise using z as the axis"
  (let ((radians (/ (* degrees pi) 180)))
    (setf (aref transform 0 0) (cos radians)
          (aref transform 0 1) (- 0 (sin radians))
          (aref transform 1 0) (sin radians)
          (aref transform 1 1) (cos radians))))
