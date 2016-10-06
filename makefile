main:
	ocamlc -c syntax.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c type.ml
	ocamlc -c coffee.ml
	ocamlc -o coffee syntax.cmo lexer.cmo parser.cmo type.cmo coffee.cmo

clean:
	rm *.cmo
	rm *.cmi
	rm lexer.ml
	rm parser.ml
	rm parser.mli
	rm coffee
