#!/bin/bash
#compiler:

ocamllex lexer.mll
ocamlyacc parser.mly
ocamlc -o bench implementation.mli implementation.ml parser.mli parser.ml lexer.ml bench.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo
