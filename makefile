objects := display.lisp matrix.lisp draw.lisp parser.lisp main.lisp
compile-lisps := --eval '(progn $(foreach file,$(objects),(compile-file "$(file)")))'
load-fasls := $(foreach file,$(objects),--load "$(subst lisp,fasl,$(file))")
run := (main-test) (main "script" 500)

all: main.fasl
	sbcl --noinform --non-interactive $(load-fasls) --eval '(progn $(run))'

main.fasl: $(objects)
	sbcl --non-interactive $(compile-lisps) > /dev/null

clean:
	rm -f *~ *.fasl *.ppm *.png
