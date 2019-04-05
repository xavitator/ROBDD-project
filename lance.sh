#!/bin/bash
#compiler:

ocamlfind ocamlopt -linkpkg -package ocamlgraph -o implementation dot.ml implementation.mli implementation.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo

#Lance le jeu
 
if [ "$#" -gt "0" ]
echo 
then ./implementation &

fi
