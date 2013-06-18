# Oqurum, a toy language
#
# Copyright (C) 2012  Martin Keegan
#
# This programme is free software; you may redistribute and/or modify
# it under the terms of the GNU Affero Public License v3.0

TGT := oq
OCAMLC := ocamlc -annot -g

$(TGT): ops.cmo parser.cmo lexer.cmo main.ml
	$(OCAMLC) -o $@ ops.ml parser.ml lexer.ml main.ml

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

ops.cmo ops.cmi: ops.ml
	$(OCAMLC) -c ops.ml

parser.cmo parser.cmi: ops.cmi parser.ml
	$(OCAMLC) -c parser.mli parser.ml

lexer.cmo: parser.cmi lexer.ml
	$(OCAMLC) -c lexer.ml



.PHONY: clean test
clean:
	rm -f -- *.mli *.cmo *.cmi parser.ml lexer.ml $(TGT) a.out *~ *annot

test: $(TGT)
	./$(TGT) < tests.oq
