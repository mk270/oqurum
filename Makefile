# Oqurum, a toy language
#
# Copyright (C) 2012  Martin Keegan
#
# This programme is free software; you may redistribute and/or modify
# it under the terms of the GNU Affero Public License v3.0

TGT := oq
OCAMLC := ocamlc -annot -g

$(TGT): eval.cmo parser.cmo keywords.cmo lexer.cmo main.ml
	$(OCAMLC) -o $@ ast.ml eval.ml parser.ml keywords.ml lexer.ml main.ml

lexer.ml: keywords.cmo

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

eval.cmo eval.cmi: ast.cmi eval.ml
	$(OCAMLC) -c eval.ml

ast.cmo ast.cmi: ast.ml
	$(OCAMLC) -c ast.ml

parser.cmo parser.cmi: eval.cmi ast.cmi parser.ml
	$(OCAMLC) -c parser.mli parser.ml

lexer.cmo: parser.cmi keywords.cmi lexer.ml
	$(OCAMLC) -c lexer.ml

keywords.cmo: keywords.ml
	$(OCAMLC) -c keywords.ml

keywords.ml: keywords.txt
	./genkeywords < $< > $@


.PHONY: clean test
clean:
	rm -f -- *.mli *.cmo *.cmi parser.ml lexer.ml $(TGT) a.out \
	         *~ *annot keywords.ml

test: $(TGT)
	./$(TGT) < tests.oq
