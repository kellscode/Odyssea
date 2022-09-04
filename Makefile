.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

start:
	./_build/default/bin/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f odyssea.zip
	zip -r odyssea.zip . -x@exclude.lst

clean:
	dune clean
	rm -f odyssea.zip

doc:
	dune build @doc

cloc: clean
	cloc --by-file --include-lang=OCaml . 