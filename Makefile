all: oplay.native ocompyuv.native

oplay.byte: stdsizes.ml oplay.ml
	ocamlbuild -use-ocamlfind oplay.byte

oplay.native: stdsizes.ml oplay.ml
	ocamlbuild -use-ocamlfind oplay.native

ocompyuv.native: ocompyuv.ml
	ocamlbuild -use-ocamlfind ocompyuv.native

clean:
	ocamlbuild -clean

