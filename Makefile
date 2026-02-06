.PHONY: all build transpile compile64 clean

TRANSPILER = ./_build/default/bin/main.exe
SEXPR64 = ../cake-sexpr-64
SEXPR32 = ../cake-sexpr-32

all: build transpile

build:
	dune build

transpile: build
	$(TRANSPILER) $(SEXPR64) > generated/cake64.ml

transpile32: build
	$(TRANSPILER) $(SEXPR32) > generated/cake32.ml

clean:
	dune clean
	rm -f generated/*.ml
