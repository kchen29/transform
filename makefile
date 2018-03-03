asdf := (asdf:load-system "engine")
main := (main-test) (main "script")

all:
	sbcl --noinform --non-interactive --load "load.lisp" --eval '(progn $(asdf) $(main))'

clean:
	rm -f *~ *.fasl *.ppm *.png
