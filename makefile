main := (main-test) (main "script")

all:
	sbcl --noinform --non-interactive --load "load.lisp" --eval '(progn $(main))'

clean:
	rm -f *~ *.fasl *.ppm *.png
