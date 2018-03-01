(defun parse-file (filename edges transform screen screen-size)
  "Parses FILENAME. Uses EDGES and TRANSFORM matrices to store edges
   and the transform matrix. Commands write to SCREEN.
   The file follows the following format:
     Every command is a single string that takes up a line
     Any command that requires arguments must have those arguments in the second line.
     The commands are as follows:
         line: add a line to the edge matrix -
	    takes 6 arguments (x0 y0 z0 x1 y1 z1)
	 ident: set the transform matrix to the identity matrix -
	 scale: create a scale matrix,
	    then multiply the transform matrix by the scale matrix -
	    takes 3 arguments (sx sy sz)
	 translate: create a translation matrix,
	    then multiply the transform matrix by the translation matrix -
	    takes 3 arguments (tx ty tz)
	 rotate: create an rotation matrix,
	    then multiply the transform matrix by the rotation matrix -
	    takes 2 arguments (axis theta) axis should be x, y or z.
            Theta is in degrees
	 apply: apply the current transformation matrix to the
	    edge matrix
	 display: draw the lines of the edge matrix to the screen
	    display the screen
	 save: draw the lines of the edge matrix to the screen
	    save the screen to a file -
	    takes 1 argument (filename)
	 quit: end parsing"
  (with-open-file (stream filename)
    (do ((line (next-line stream) (next-line stream)))
        ((string= line "quit"))
      (switch line #'string=
        ("line")
        ("ident")
        ("scale")
        ("translate")
        ("rotate")))))

(defun next-line (stream)
  "Reads the next line in stream. Returns \"quit\" if eof is reached"
  (read-line stream nil "quit"))

(defmacro switch (value test &body cases)
  "Macro for switch-case statements."
  `(cond
     ,@(loop for (test-value return-value) in cases
          if (eql 'otherwise test-value)
            collect `(t ,return-value)
          else
            collect `((funcall ,test ,value ,test-value) ,return-value))))
