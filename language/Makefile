main:
	ocamlbuild -use-menhir -yaccflag --explain -pkgs str main.byte

test:
	ocamlbuild -use-menhir -pkgs oUnit,str,unix test_main.byte && ./test_main.byte

clean:
	ocamlbuild -clean
