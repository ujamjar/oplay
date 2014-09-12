all: oplay.native

oplay.byte: stdsizes.ml oplay.ml
	ocamlbuild -use-ocamlfind oplay.byte

oplay.native: stdsizes.ml oplay.ml
	ocamlbuild -use-ocamlfind oplay.native

clean:
	ocamlbuild -clean

