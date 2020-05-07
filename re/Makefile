.PHONY: all
all: re fa

re: regular.ml relexer.mll reparser.mly pattern.ml re.ml
	ocamlyacc reparser.mly
	ocamllex relexer.mll
	ocamlopt -o re regular.ml reparser.mli relexer.ml reparser.ml pattern.ml re.ml

fa: regular.ml falexer.mll faparser.mly fa.ml
	ocamlyacc faparser.mly
	ocamllex falexer.mll
	ocamlopt -o fa regular.ml faparser.mli falexer.ml faparser.ml fa.ml

.PHONY: clean
clean:
	rm -f *.cmo *.cmi *.cmx *.o *.mli re fa falexer.ml faparser.ml relexer.ml reparser.ml
