#!/bin/bash
#compiler:

ocamlfind ocamlopt -linkpkg -package ocamlgraph -o sudoku dot.ml implementation.mli implementation.ml sudoku.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo
