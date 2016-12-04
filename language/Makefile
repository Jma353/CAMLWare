# Makefile for language components of the project

main:
	ocamlbuild -use-menhir main.byte

test:
	ocamlbuild -use-menhir -pkgs oUnit,unix test_main.byte && ./test_main.byte

clean:
	ocamlbuild -clean
