# ROBDD-project

Le but de ce projet est d'implémenter et d'utiliser un algorithme permettant de construire le ROBDD –canoniquement- associé à une expression booléenne donnée.
On appelle ici un ROBDD un diagramme decisionnel binaire ordonné et réduit (reduced ordered binary decision diagram).

Nous nous basons sur l'article [An Introduction to Binary Decision Diagrams](https://www.cmi.ac.in/~madhavan/courses/verification-2011/andersen-bdd.pdf) de Henrik Reif Andersen et sur l'article [Graph-Based Algorithms
for Boolean Function Manipulation](http://www.cs.cmu.edu/~bryant/pubdir/ieeetc86.pdf) de Randal E. Bryant.

Dans un premier temps, nous implémenterons le package pour obtenir un ROBDD.
Ensuite, nous utiliserons ce package dans des exemples : le problème des N-reines,
circuits combinatiores, etc

## Package ROBDD

**Init(*****n*****)** : Initialisation du package. Utiliser *n* variables numérotées jusqu'à *n*.

**Print(*****u*****)** : Affiche une représentation d'une ROBDD sur la sortie standard. Utile pour le debug.

**Mk(*****i,l,h*****)** : Retourne le nombre *u* de la node avec *var(u)=i, low(u)=l, high(u)=h*. Cela peut être une node existante, ou une node qui vient d'être créé. On s'assure par cette fonction de la stabilité la forme canonique de la ROBDD.

**Build(*****t*****)** : Construction un ROBDD correspondant à l'expression booléenne *t* donnée en argument. On accepte le quantificateur existentiel et le quantificateur universel.

**Apply(*****op, u1, u2*****)** : Construction d'un ROBDD résultant de l'application de l'opérateur *op* sur la ROBDD *u1* et *u2*.

**Restrict(*****u, j, b*****)** : Retourne la restriction de la variable *j* par la valeur de vérité *b*. Cela correspond à *u[ b/j ]*.

**SatCount(*****u*****)** : Retourne le nombre de valeur de vérité différentes possibles pour le ROBDD *u*.(Utiliser un type suffisamment large par contenir de très grands nombres, comme un float.)

**AnySat(*****u*****)** : Retourne une valeur de vérité pour le ROBDD *u*.

**AllSat(*****u*****)** : Retourne toutes les valeurs de vérité pour le ROBDD *u*.

**Simplify(*****u*****)** : Simplifie un ROBDD *u* en essayant de supprimer des noeuds. Si on obtient le même ROBDD, c'est qu'il n'y a pas de simplification possible.

**Subsitute(*****u1, i, u2*****)** : Retourne *u1* où toutes les occurences libres de la variable *i* dans *u1* sont substitués par *u2*. Cela correspond à *u1[ u2/i ]*.

## Exemples de problèmes résolus avec un ROBDD

### Le problème des N dames

### Sudoku

### Benchmarks

## Représentation graphique des ROBDD

Lors de l'utilisation de notre algorithme, on génère un graphe.

On peut obtenir le rendu de ce graphe. Pour cela, on a besoin d'avoir le package `ocamlgraph` d'installer sur son ordinateur.

## Compilation 

Il faut au préalable vérifier que `make` est installé.

Ensuite, il vous faut télécharger le dépot, puis lancer la commande :

```
    make help
```

Et vous suivez les instructions fournies.