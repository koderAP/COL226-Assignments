target:
	@ocamlc -c ast.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -o my_program parser.ml lexer.ml ast.ml main.ml
	@./my_program
	@rm -f *.cmi
	@rm -f *.cmo *.mli
	@rm -rf my_program main parser.ml lexer.ml