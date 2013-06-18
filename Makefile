
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
