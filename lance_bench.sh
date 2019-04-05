#!/bin/bash
#compiler:

ocamllex lexer.mll
ocamlyacc parser.mly
ocamlfind ocamlopt -linkpkg -package ocamlgraph -o bench dot.ml implementation.mli implementation.ml parser.mli parser.ml lexer.ml bench.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo *.cmx *.o
