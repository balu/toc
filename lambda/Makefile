.PHONY: all
all: lambda

lambda: lambda.ml lexer.mll parser.mly main.ml
	ocamlyacc parser.mly
	ocamllex lexer.mll
	ocamlopt -o lambda lambda.ml parser.mli lexer.ml parser.ml main.ml

.PHONY: clean
clean:
	rm -f *.cmo *.cmi *.cmx *.o *.mli lambda lexer.ml parser.ml
