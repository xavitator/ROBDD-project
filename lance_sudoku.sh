#!/bin/bash
#compiler:

ocamlc -o sudoku implementation.ml sudoku.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo
