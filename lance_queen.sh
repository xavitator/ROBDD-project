#!/bin/bash
#compiler:

ocamlfind ocamlopt -linkpkg -package ocamlgraph -o queen dot.ml implementation.mli implementation.ml queen.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo
