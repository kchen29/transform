objects := display.lisp matrix.lisp draw.lisp main.lisp
compile-lisps := --eval '(progn $(foreach file,$(objects),(compile-file "$(file)")))'
load-fasls := $(foreach file,$(objects),--load "$(subst lisp,fasl,$(file))")

all: main.fasl
	sbcl --noinform --non-interactive $(load-fasls) --eval '(progn (main-test) (main 500 "output.ppm"))'

main.fasl: $(objects)
	sbcl --non-interactive $(compile-lisps) > /dev/null

clean:
	rm -f *~ *.fasl *.ppm
