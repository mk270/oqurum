
OCAMLC := ocamlc -annot

exe: ops.cmo parser.cmo lexer.cmo main.ml
	$(OCAMLC) -o $@ ops.ml parser.ml lexer.ml main.ml

lexer.ml: lexer.mll
	ocamllex $<

parser.ml: parser.mly
	ocamlyacc $<

ops.cmo: ops.ml
	$(OCAMLC) -c ops.ml

parser.cmo: parser.ml
	$(OCAMLC) -c parser.mli parser.ml

lexer.cmo: lexer.ml
	$(OCAMLC) -c lexer.ml

.PHONY: clean test
clean:
	rm -f -- *.mli *.cmo *.cmi parser.ml lexer.ml exe a.out *~ *annot

test: exe
	./exe < sample.ush
