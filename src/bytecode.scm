;;;;;;
;;;;;; SCompiler / Code octet (bytecode) de la VM
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE)

#lang racket

;; on exporte tout
(provide (all-defined-out))

;;; Valeurs entières des types
(define type-codes '((UNIT    0)
                     (INT     1)
                     (PRIM    2)
                     (FUN     3)
                     (BOOL    4)
                     (PAIR    5)))

;;; Valeurs entières des op-codes
(define op-codes '((LABEL -1)
                   (GALLOC 0)
                   (PUSH 1)
                   (GSTORE 2)
                   (POP 3)
                   (JUMP 4)
                   (GFETCH 5)
                   (CALL 6)
                   (RETURN 7)
                   (FETCH 8)
                   (JFALSE 9)
                   (STORE 10)
                   (ERROR 11)
                   (ALLOC 12)
                   (DELETE 13)))


;;;;; Codage des valeurs
; type BCValue
; ('INT <num>) valeur numérique
; ('BOOL <bool>) valeur booléenne
; ('UNIT) non valeur
; ('FUN <symbol>) valeur fonctionnelle (étiquette du code)
; ('PRIM <num>) primitives (son numéro)

;;;; Constructeurs
;;; BC-int : Int -> BCValue
(define (BC-int x)
  (list 'INT x))

;;; BC-bool : Bool -> BCValue
(define (BC-bool b)
  (list 'BOOL b))

;;; BC-unit : Unit -> BCValue
(define (BC-unit)
  (list 'UNIT))

;;; BC-fun : Symbol -> BCValue
(define (BC-fun lbl)
  (list 'FUN lbl))

;;; BC-prim : Int -> BCValue
(define (BC-prim n)
  (list 'PRIM n))

;;;; Reconnaisseurs
;;; BC-int? : BCValue -> Bool
(define (BC-int? bcval)
  (eq? (car bcval) 'INT))

;;; BC-bool? : BCValue -> Bool
(define (BC-bool? bcval)
  (eq? (car bcval) 'BOOL))

;;; BC-unit? : BCValue -> Bool
(define (BC-unit? bcval)
  (eq? (car bcval) 'UNIT))

;;; BC-fun? : BCValue -> Bool
(define (BC-fun? bcval)
  (eq? (car bcval) 'FUN))

;;; BC-prim? : BCValue -> Bool
(define (BC-prim? bcval)
  (eq? (car bcval) 'PRIM))

;;;; Accesseurs
;;; BC-int-val : BCValue -> Int
(define (BC-int-val bcval)
  (cadr bcval))

;;; BC-bool-val : BCValue -> Bool
(define (BC-bool-val bcval)
  (cadr bcval))

;;; BC-fun-label : BCValue -> Symbol
(define (BC-fun-label bcval)
  (cadr bcval))

;;; BC-prim-nbr : BCValue -> Int
(define (BC-prim-nbr bcval)
  (cadr bcval))

;;;;; JEU D'INSTRUCTIONS
; type BCInstr
;; (NOOP)
;  instruction sans effet (Non Operation).
;; (PUSH <val>)
;  place au sommet de la pile la valeur <val>.
;; (POP)
;  retire l'élément au sommet de la pile.
;; (GALLOC)
;  alloue une place nouvelle place dans l'environnement global.
;; (GSTORE <ref>)
;  dépile la valeur en sommet de la pile et l'enregistre dans
;  l'environnement global à la position <ref> (entier positif
;  ou nul).
;; (GFETCH <ref>)
;  empile la valeur de l'environnement global à la position <ref>
;  (entier positif ou nul).
;; (STORE <ref>)
;  dépile la valeur en sommet de la pile pour la placer à la
;  position <ref> (entier positif ou nul).
;; (FETCH <ref>)
;  empile une copie de la valeur de la pile située à la position
;  <ref> (entier positif ou nul) dans l'environnement lexical.
;; (CALL <nb>)
;  appelle la procédure (fonction/primitive) en sommet de pile;
;  avec <nb> nombre d'arguments de la procédure appelée.
;  Une fenêtre de pile est créée avec, notamment, l'adresse de
;  retour. Au retour de la procédure, la valeur calculée est
;  sur le sommet de la pile.
;; (JUMP <label>)
;  effectue un saut non conditionnel à l'étiquette <label>.
;; (JFALSE <label>)
;; effectue un saut conditionnel à l'étiquette <label> si
;  le sommet de pile est la valeur FALSE. Le sommet de pile
;  est consommé.
;; (LABEL <label>)
;  déclare une position/étiquette de saut avec la référence <label>.
;; (RETURN)
;  instruction de retour d'appel de procédure. L'adresse/étiquette
;  de retour et le nombre d'arguments pour le nettoyage sont sur
;  la pile.

;;;; Constructeurs
;;; BC-NOOP : Unit -> BCInstr
(define (BC-NOOP)
  (list 'NOOP))

;;; BC-PUSH : BCValue -> BCInstr
(define (BC-PUSH val)
  (list 'PUSH val))

;;; BC-POP : Unit -> BCInstr
(define (BC-POP)
  (list 'POP))

;;; BC-GALLOC : Unit -> BCInstr
(define (BC-GALLOC)
  (list 'GALLOC))

;;; BC-GSTORE : Int -> BCInstr
(define (BC-GSTORE ref)
  (list 'GSTORE ref))

;;; BC-GFETCH : Int -> BCInstr
(define (BC-GFETCH ref)
  (list 'GFETCH ref))

;;; BC-STORE : Int -> BCInstr
(define (BC-STORE ref)
  (list 'STORE ref))

;;; BC-FETCH : Int -> BCInstr
(define (BC-FETCH ref)
  (list 'FETCH ref))

;;; BC-CALL : Int -> BCInstr
(define (BC-CALL n)
  (list 'CALL n))

;;; BC-JUMP : Symbol -> BCInstr
(define (BC-JUMP lbl)
  (list 'JUMP lbl))

;;; BC-JFALSE : Symbol -> BCInstr
(define (BC-JFALSE lbl)
  (list 'JFALSE lbl))

;;; BC-LABEL : Symbol -> BCInstr
(define (BC-LABEL lbl)
  (list 'LABEL lbl))

;;; BC-RETURN : Unit -> BCInstr
(define (BC-RETURN)
  (list 'RETURN))

;;; BC-ERROR : Unit -> BCInstr
(define (BC-ERROR)
  (list 'ERROR))

;;; BC-ALLOC : Int -> BCInstr
(define (BC-ALLOC n)
  (list 'ALLOC n))

;;; BC-DELETE : Int -> BCInstr
(define (BC-DELETE n)
  (list 'DELETE n))

;;;; Reconnaisseurs
;;; BC-sym? : Symbol * BCInstr -> Bool
(define (BC-sym? sym instr)
  (eq? (car instr) sym))

;;;BC-NOOP? : BCInstr -> Bool
(define (BC-NOOP? instr)
  (BC-sym? 'NOOP instr))

;;; BC-PUSH? : BCInstr -> Bool
(define (BC-PUSH? instr)
  (BC-sym? 'PUSH instr))

;;; BC-POP? : BCInstr -> Bool
(define (BC-POP? instr)
  (BC-sym? 'POP instr))

;;; BC-GALLOC? : BCInstr -> Bool
(define (BC-GALLOC? instr)
  (BC-sym? 'GALLOC instr))

;;; BC-GSTORE? : BCInstr -> Bool
(define (BC-GSTORE? instr)
  (BC-sym? 'GSTORE instr))

;;; BC-GFETCH? : BCInstr -> Bool
(define (BC-GFETCH? instr)
  (BC-sym? 'GFETCH instr))

;;; BC-STORE? : BCInstr -> Bool
(define (BC-STORE? instr)
  (BC-sym? 'STORE instr))

;;; BC-FETCH? : BCInstr -> Bool
(define (BC-FETCH? instr)
  (BC-sym? 'FETCH instr))

;;; BC-CALL? : BCInstr -> Bool
(define (BC-CALL? instr)
  (BC-sym? 'CALL instr))

;;; BC-JUMP? : BCInstr -> Bool
(define (BC-JUMP? instr)
  (BC-sym? 'JUMP instr))

;;; BC-JFALSE? : BCInstr -> Bool
(define (BC-JFALSE? instr)
  (BC-sym? 'JFALSE instr))

;;; BC-LABEL? : BCInstr -> Bool
(define (BC-LABEL? instr)
  (BC-sym? 'LABEL instr))

;;; BC-RETURN? : BCInstr -> Bool
(define (BC-RETURN? instr)
  (BC-sym? 'RETURN instr))

;;; BC-ERROR? : BCInstr -> Bool
(define (BC-ERROR? instr)
  (BC-sym? 'ERROR instr))

;;; BC-ALLOC? : BCInstr -> Bool
(define (BC-ALLOC? instr)
  (BC-sym? 'ALLOC instr))

;;; BC-DELETE? : BCInstr -> Bool
(define (BC-DELETE? instr)
  (BC-sym? 'DELETE instr))

;;;; Accesseurs
;;; BC-PUSH-value : BCInstr -> BCValue
(define (BC-PUSH-value instr)
  (cadr instr))

;;; BC-GSTORE-ref : BCInstr -> Int
(define (BC-GSTORE-ref instr)
  (cadr instr))

;;; BC-GFETCH-ref : BCInstr -> Int
(define (BC-GFETCH-ref instr)
  (cadr instr))

;;; BC-STORE-ref : BCInstr -> Int
(define (BC-STORE-ref instr)
  (cadr instr))

;;; BC-FETCH-ref : BCInstr -> Int
(define (BC-FETCH-ref instr)
  (cadr instr))

;;; BC-CALL-nbarg : BCInstr -> Int
(define (BC-CALL-nbarg instr)
  (cadr instr))

;;; BC-JUMP-label : BCInstr -> Symbol
(define (BC-JUMP-label instr)
  (cadr instr))

;;; BC-JFALSE-label : BCInstr -> Symbol
(define (BC-JFALSE-label instr)
  (cadr instr))

;;; BC-LABEL-label : BCInstr -> Symbol
(define (BC-LABEL-label instr)
  (cadr instr))

;;; BC-ALLOC-ref : BCInstr -> Int
(define (BC-ALLOC-ref instr)
  (cadr instr))

;;; BC-DELETE-ref : BCInstr -> Int
(define (BC-DELETE-ref instr)
  (cadr instr))