main:
	ocamlbuild -pkgs str main.byte

test:
	ocamlbuild -pkgs oUnit,str,unix bitstream_test.byte && ./bitstream_test.byte

clean:
	ocamlbuild -clean
