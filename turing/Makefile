.PHONY: all
all: tm

tm: tm.ml lexer.mll parser.mly main.ml
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlopt -o tm tm.ml parser.mli lexer.ml parser.ml main.ml

.PHONY: clean
clean:
	rm -f *.cmo *.cmi *.cmx *.o *.mli tm lexer.ml parser.ml
