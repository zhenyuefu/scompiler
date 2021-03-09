;;;;;;
;;;;;; SCompiler/Utilitaires
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE) 

;;; on utilise le sous-ensemble Scheme du langage racket
#lang racket

;;; exportation des fonctions (spécifique à racket)
(provide print
         println
         emit-error)

;;; display-all: SchemeExpr * ... -> Unit
;;; affiche une suite de valeurs
(define (print . args)
  (for-each display args))

;;; println: SchemeExpr * ... -> Unit
;;; affiche une suite de valeurs puis un (newline)
(define (println . args)
  (apply print args)
  (newline))

;;; emit-error: SExpr * ... -> Unit
;;; (emit-error e1 ...) émet un message d'erreur
(define (emit-error . args)
  (display "Error: ")
  (for-each display args)
  (newline)
  (error "" "<*-fatal-error-*>" '()))


