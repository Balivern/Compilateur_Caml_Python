Projet Récursion et Itération


Projet réalisé par Lola Condom, Maxime Rieux, Renan Cabane.

Répartition du travail :

Lola :
•	Écriture de fiches d’aide explicatives concernant les différentes fonctions à réaliser et les étapes clés du projet
•	Écriture à l’aide d’IA de certaines fonctions pour donner une base de travail et de compréhension pour Maxime et Renan
•	Fichiers tests

Maxime :
•	Fichier typing.ml : 
-	tp_application
-	trouver (fonction auxiliaire de la fonction tp_var_funbind)
-	tp_expr (cas BinOp)

Renan :
•	Fichier typing.ml : 
-	tp_var_local_var
-	tp_var_funbind 
-	tp_var
-	tp_expr (cas Const, VarE, IfThenElse, CallE, _)

Maxime + Renan :
•	Fichier typing.ml : 
-	tp_funbind
-	tp_prog
•	Fichier transf.ml :
-	containsRecursiveCall
-	is_tailrec_expr
-	transf_expr
 
Problèmes rencontrés :

•	Peu d’expérience pratique de programmation en Caml : impliquant une faible connaissance des fonctions (notamment celles du module List) de Caml.
•	Difficultés rencontrées dans la compréhension du sujet du projet.
•	Difficultés rencontrées dans l’installation et l’utilisation de opam : nous n’avions pas les droits requis sur les ordinateurs de la bibliothèque universitaire pour effectuer les commandes opam, et nous n’avons pas réussi à installer Lex sur nos ordinateurs sous Windows. D’où la difficulté à tester nos différentes fonctions.
