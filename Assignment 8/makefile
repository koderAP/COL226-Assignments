make:
	@ocamlc -c ast.ml
	@ocamlyacc parser.mly
	@ocamlc -c parser.mli
	@ocamlc -c parser.ml
	@ocamllex lexer.mll
	@ocamlc -c lexer.ml
	@ocamlc -c main.ml
	@ocamlfind ocamlc -linkpkg -package unix -o prolog ast.cmo parser.cmo lexer.cmo main.cmo
	@rm -f lexer.ml parser.ml parser.mli
	@rm -f *.c*
	@./prolog
	@rm -f prolog 
	

clean:
	rm -f lexer.ml parser.ml parser.mli
	rm -f *.c*
	rm -f prolog
