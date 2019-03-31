#!/bin/bash
#compiler:

ocamlc -o sudoku implementation.mli implementation.ml sudoku.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo
