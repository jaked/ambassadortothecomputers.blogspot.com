to_of_string.cmo: to_of_string.ml
	ocamlc.opt -I +camlp4 -c -pp camlp4orf  to_of_string.ml
	#ocamlfind ocamlc -package camlp4.quotations.r -syntax camlp4o -c to_of_string.ml

clean:
	rm -f *.cmo *.cmi *~ \#*
