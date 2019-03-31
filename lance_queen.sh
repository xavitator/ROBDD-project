#!/bin/bash
#compiler:

ocamlc -o queen dot.ml implementation.mli implementation.ml queen.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo
