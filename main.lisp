(defun main-test ()
  "Tests make-transformation."
  (format t "translate matrix 3 4 5")
  (print-matrix (make-translate 3 4 5))
  (format t "scale matrix 1.5 1.6 1.7")
  (print-matrix (make-scale 1.5 1.6 1.7))
  (format t "rotation matrix z 45")
  (print-matrix (make-rotate-z 45)))
  
(defun main (filename screen-size)
  "Sets up then parses FILENAME. See parser.lisp."
  (let* ((dimensions (list screen-size screen-size))
         (screen (make-array dimensions :initial-element '(0 0 0)))
         (edges (make-matrix 4 0))
         (transform (make-matrix)))
    (parse-file filename edges transform screen screen-size)))
