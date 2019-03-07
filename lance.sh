#!/bin/bash
#compiler:

ocamlc -o implementation implementation.ml

#effacer les fichiers auxilliaires :

rm -f *.cmi *.cmo

#Lance le jeu
 
if [ "$#" -gt "0" ]
echo 
then ./implementation &

fi
