(defun main-test ()
  "Tests matrix stuff."
  (let ((edge-list (make-matrix 4 0))
        (transform (make-matrix)))
    (format t "transform matrix:")
    (print-matrix transform)

    (format t "turn transform matrix into identity:")
    (to-identity transform)
    (print-matrix transform)

    (format t "add an edge to the edge matrix")
    (add-edge edge-list 0 250 0 499 0 0)
    (print-matrix edge-list)
    
    (format t "multiply the edge matrix with the transform matrix")
    (setf edge-list (matrix-multiply transform edge-list))
    (print-matrix edge-list)
    
    (format t "turn the transform matrix into a nontrivial transform matrix:")
    (rotatef (aref transform 0 0) (aref transform 1 0) (aref transform 1 1) (aref transform 0 1))
    (print-matrix transform)

    (format t "apply the transform to the edge list")
    (setf edge-list (matrix-multiply transform edge-list))
    (print-matrix edge-list)))


(defun draw-transform-draw (matrix transform screen color)
  "Draws the MATRIX, does a TRANSFORM, then draws again."
  (draw-lines matrix screen color)
  (matrix-multiply transform matrix)
  (draw-lines matrix screen color))

(defmacro add-edges (matrix edges)
  "Add multiple EDGES to the MATRIX."
  `(progn ,@(loop for edge in edges
               collect `(add-edge ,matrix
                                  ,(first edge) ,(second edge) 0
                                  ,(third edge) ,(fourth edge) 0))))

(defmacro clear-add-draw (matrix edges transform screen color)
  "Clear the MATRIX, add EDGES, then draw with TRANSFORM."
  `(progn (clear-matrix ,matrix)
          (add-edges ,matrix ,edges)
          (draw-transform-draw ,matrix ,transform ,screen ,color)))

(defun translate (delx dely matrix)
  "Translates MATRIX by DELX and DELY"
  (let ((transform (make-matrix)))
    (to-identity transform)
    (setf (aref transform 0 3) delx
          (aref transform 1 3) dely)
    (matrix-multiply transform matrix)))

(defun rotate (radians matrix)
  "Rotates MATRIX by RADIANS counter-clockwise"
  (let ((transform (make-matrix)))
    (to-identity transform)
    (setf (aref transform 0 0) (cos radians)
          (aref transform 0 1) (- 0 (sin radians))
          (aref transform 1 0) (sin radians)
          (aref transform 1 1) (cos radians))
    (matrix-multiply transform matrix)))

(defun dilate (factor matrix)
  "Dilates MATRIX by FACTOR"
  (let ((transform (make-matrix)))
    (to-identity transform)
    (setf (aref transform 0 0) factor
          (aref transform 1 1) factor)
    (matrix-multiply transform matrix)))

(defun main (a-size filename)
  "Make fancy A-SIZE by A-SIZE image. Outputs to FILENAME."
  (let* ((dimensions (list a-size a-size))
         (half (/ a-size 2))
         (full (1- a-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (matrix (make-matrix 4 0))
         (transform (make-matrix)))
    ;;make transform rotate 22.5 degrees from the point (250, 250),
    ;;and dilate by a small factor (fill the screen)
    (to-identity transform)
    (translate -250 -250 transform)
    (rotate (/ (* 22.5 pi) 180) transform)
    (dilate 1.0824 transform)
    (translate 250 250 transform)
    
    (clear-add-draw matrix ((0 0 full full)
                           (0 0 full half)
                           (full full 0 half))
                    transform screen '(0 255 0))

    (clear-add-draw matrix ((0 full full 0)
                           (0 full full half)
                           (full 0 0 half))
                    transform screen '(0 255 255))

    (clear-add-draw matrix ((0 0 half full)
                           (full full half 0))
                    transform screen '(255 0 0))

    (clear-add-draw matrix ((0 full half 0)
                           (full 0 half full))
                    transform screen '(255 0 255))

    (clear-add-draw matrix ((0 half full half)
                           (half 0 half full))
                    transform screen '(255 255 0))
    
    (write-ppm filename dimensions screen)
    (display filename)))
