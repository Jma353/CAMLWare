main:
	ocamlbuild -use-menhir -yaccflag --explain -pkgs str main.byte

render:
	ocaml setup.ml -configure && ocaml setup.ml -build

test:
	ocamlbuild -pkgs oUnit,str,unix bitstream_test.byte && ./bitstream_test.byte

clean:
	ocamlbuild -clean
