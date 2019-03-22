#!/bin/bash
#compiler:

ocamlc -o queen implementation.ml queen.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo
