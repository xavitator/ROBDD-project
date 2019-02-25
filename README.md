# ROBDD-project
Le but de ce projet est d'implémenter et d'utiliser un algorithme permettant de construire le ROBDD –canoniquement- associé à une expression booléenne donnée.

Nous nous basons sur l'article [An Introduction to Binary Decision Diagrams](https://www.cmi.ac.in/~madhavan/courses/verification-2011/andersen-bdd.pdf) de Henrik Reif Andersen et sur l'article [Graph-Based Algorithms
for Boolean Function Manipulation](http://www.cs.cmu.edu/~bryant/pubdir/ieeetc86.pdf) de Randal E. Bryant.

Dans un premier temps, nous implémenterons le package pour obtenir un ROBDD.
Ensuite, nous utiliserons ce package dans des exemples : le problème des N-reines,
circuits combinatiores, etc

# Package ROBDD

**Init(*****n*****)** : Initialisation du package. Utiliser *n* variables numérotées jusqu'à *n*.

**Print(*****u*****)** : Affiche une représentation d'une ROBDD sur la sortie standard. Utile pour le debug.

**Mk(*****i,l,h*****)** : Return the number *u* of a node with *var(u)=i, low(u)=l, high(u)=h*. This could be an existing node, or a newly created node. The reducedness of the ROBDD should not be violated.

**Build(*****t*****)** : Construct an ROBDD from a Boolean expression. We accept existancial quantification.

**Apply(*****op, u1, u2*****)** : Construct the ROBDD resulting from applying *op* on *u1* and *u2*.

**Restrict(*****u, j, b*****)** : Restrict the ROBDD *u* according to the truth assignment [ *b*/*j* ]

**SatCount(*****u*****)** : Return the number of elements in the set *sat(u)* (Use a type that can contain very large numbers such as floating point numbers.)

**AnySat(*****u*****)** : Return a satisfying truth assignment for *u*.

**AllSat(*****u*****)** : Return all satisfying truth assignment for *u*.

**Simplify(*****u*****)** : simplify an ROBDD by trying to remove nodes.

**Subsitute(*****u1, i, u2*****)** : Return *u1* substituting all free occurrences of *i* in *u1* by *u2*.

# Examples of problem solving with ROBDDs

## The 8 Queens problem

## Correctness of Combinational Circuits

## Equivalence of Combinational Circuits
