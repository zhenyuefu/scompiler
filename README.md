# Compilateur (Mini-)Scheme

Ce dépôt contient les sources du compilateur (Mini-)Scheme
du cours de compilation de licence 3 de Sorbonne Université

## Compilation du compilateur (avec racket)

Pour compiler le code du compilateur avec racket,
  il suffit d'aller dans le répertoire `src/` et
  d'invoquer la commande suivante :
  
```shell
$ raco exe scompiler.scm
```

Cela va générer un executable `scompiler`.

Il est également possible de créer l'exécutable du compilateur
depuis l'interface drracket.

**Attention** : le code du compilateur est globalement du Scheme
R5RS avec les différences suivantes :

 - la déclaration `#lang racket` au début des sources
 - les déclarations `(provide ...)` et `(require ...)`
 - les paires immutables par défaut et les paires mutables
 explicites avec `mpair?`, `mcons`, `mcar` et `mcdr`.
 - l'utilisation de quelques procédures spécifiques (arguments de 
 ligne de commande, entrées/sorties, manipulations de chaînes, erreurs, etc.)


## Bootstrap

TODO

## Tests unitaires

Pour lancer les tests unitaires, aller dans le répertoire `test/`

et invoquer `racket <fichier-de-test>.scm`  tout simplement.
N'hésitez pas à ajouter des fichiers de test, et compléter
les fichiers existants.

On ajoutera un script pour lancer tous les tests plus tard.


----
Copyright (C) 2021- F.P. under the GPLv3 (cf. LICENSE)
