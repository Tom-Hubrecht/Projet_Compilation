CMX=go_lexer.cmx go_parser.cmx go_printer.cmx go_compiler.cmx
PARSER_GEN=go_parser.ml go_parser.automaton go_parser.conflicts go_parser.mli
GENERATED=go_lexer.ml $(PARSER_GEN)
BIN=pgoc

all: $(BIN)

$(BIN): $(CMX)
	ocamlopt -o $(BIN) $(CMX)

.SUFFIXES: .mli .ml .cmi .cmx .mll .mly

.mli.cmi:
	ocamlopt -c $<

.ml.cmx:
	ocamlopt -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	ocamlyacc -v $<

# Don't delete .cmi files for Merlin
clean:
	rm -f *.cm[ix] *.o *~ $(BIN) $(GENERATED)

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend

.PHONY: all clean
