(defun write-ppm (filename dimensions screen)
  "Writes a ppm, assuming P3 and max color value of 255.
   Writes to FILENAME, with DIMENSIONS and 2D array of colors SCREEN."
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (format stream
            "P3 ~a ~a 255 ~{~%~{~{~a ~a ~a ~}~}~}"
            (first dimensions) (second dimensions) (screen-to-list screen))))

(defun screen-to-list (screen)
  "Turns a 2D array SCREEN into a list.
   Places (0, 0) on the lower left corner of the list."
  (loop for y from (1- (array-dimension screen 1)) downto 0
     collect (loop for x below (array-dimension screen 0)
                collect (aref screen x y))))

(defun plot (x y screen color)
  "Plots (x, y) on the 2D array SCREEN with COLOR.
   Floors x and y. Checks bounds. COLOR is not copied."
  (setf x (floor x) y (floor y))
  (when (and (< -1 x (array-dimension screen 0)) (< -1 y (array-dimension screen 1)))
    (setf (aref screen x y) color)))

(defun display (filename &key (wait nil))
  "Displays the image with FILENAME.
   If WAIT is t, then will wait until display ends
   Uses imagemagick's display to display an image."
  (run-program "display" (list filename) :wait wait :search t))

(defun write-display (filename dimensions screen)
  "Writes the ppm, displays the image, then removes it"
  (write-ppm filename dimensions screen)
  (display filename :wait t)
  (delete-file filename))
