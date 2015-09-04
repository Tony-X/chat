chat.native: chat.ml
		ocamlfind ocamlopt -thread -o chat.native -package unix,core,threads \
		-linkpkg chat.ml

clean: 
		rm chat.native chat.cmx chat.cmi  chat.o
	
