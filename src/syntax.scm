;;;;;;
;;;;;; SCompiler / Syntaxe du langage noyau
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE)

#lang racket

;; on exporte tout
(provide (all-defined-out))

;;;;;
;;;;; Le langage noyau: type KExpr, sous ensemble de SExpr
;;;;;
;  <KExpr> ::= <symbol>
;           |  <atom>                          // valeurs immédiates
;           |  ( define <symbol> <KExpr> )
;           |  ( quote <KExpr> )
;           |  ( if <KExpr> <KExpr> <KExpr> )
;           |  ( set! <symbol> <KExpr> )
;           |  ( begin <KExpr>* )
;           |  ( lambda ( <symbol>* ) <KExpr> )
;           |  ( <KExpr>+ )                   // application

;;;; Reconnaisseurs et accesseurs

;;;; Atomes
;;; atom?: KExpr -> bool
;;; Tout sauf les symboles (au sens Scheme) et les paires
(define (atom? x)
  (not (or (symbol? x) (pair? x))))

;;;; Définition: 'define
;;; define-expr?: KExpr -> bool
(define (define-expr? x)
  (eq? (car x) 'define))

;;; define-var: KExpr -> Symbol
(define (define-var expr)
  (cadr expr))

;;; define-expr: KExpr -> KExpr
(define (define-expr expr)
  (caddr expr))

;;;; Citation: 'quote
;;; quote-expr?: KExpr -> bool
(define (quote-expr? x)
  (eq? (car x) 'quote))

;;; quote-expr: KExpr -> KExpr
(define (quote-expr expr)
  (cadr expr))

;;;; Alternative: 'if
;;; if-expr?: KExpr -> bool
(define (if-expr? x)
  (eq? (car x) 'if))

;;; if-cond: KExpr -> KExpr
(define (if-cond expr)
  (cadr expr))

;;; if-then: KExpr -> KExpr
(define (if-then expr)
  (caddr expr))

;;; if-else: KExpr -> KExpr
(define (if-else expr)
  (cadddr expr))

;;;; Affectation: 'set!
;;; set!-expr?: KExpr -> bool
(define (set!-expr? x)
  (eq? (car x) 'set!))

;;; set!-sym: KExpr -> Symbol
(define (set!-sym expr)
  (cadr expr))

;;; set!-expr: KExpr -> KExpr
(define (set!-expr expr)
  (caddr expr))

;;;; Affectation: 'mset!
;;; mset!-expr?: KExpr -> bool
(define (mset!-expr? x)
  (eq? (car x) 'mset!))

;;; mset!-sym: KExpr -> <symbol>*
(define (mset!-sym expr)
  (cadr expr))

;;; mset!-expr: KExpr -> <KExpr>*
(define (mset!-expr expr)
  (caddr expr))

;;;; Séquence: 'begin
;;; begin-expr?: KExpr -> bool
(define (begin-expr? x)
  (eq? (car x) 'begin))

;;; begin-body: KExpr -> LIST[KExpr]
(define (begin-body expr)
  (cdr expr))

;;;; Fonction: 'lambda
;;; lambda-expr?: KExpr -> bool
(define (lambda-expr? x)
  (eq? (car x) 'lambda))

;;; lambda-params: KExpr -> LIST[Symbol]
(define (lambda-params expr)
  (cadr expr))

;;; lambda-body: KExpr -> LIST[KExpr]
(define (lambda-body expr)
  (cddr expr))

;;;; Alternative: 'while
;;; while-expr?: KExpr -> bool
(define (while-expr? expr)
  (eq? (car expr) 'while))

;;; while-cond: KExpr -> KExpr
(define (while-cond expr)
  (cadr expr))

;;; while-body: KExpr -> KExpr
(define (while-body expr)
  (caddr expr))

;;;; Lambdas n-aires
;;; n-aires? KExpr -> bool
(define (n-aires? expr)
  (eq? (car expr) '$))

;;; n-aires-function: KExpr -> KExpr
(define (n-aires-function expr)
  (cadr expr))

;;; n-aires-args: KExpr -> LIST[KExpr]
(define (n-aires-args expr)
  (cddr expr))

;;; lambda$?: KExpr -> bool
(define (lambda$-expr? expr)
  (eq? (car expr) 'lambda$))

;;; lambda$-params: KExpr -> Symbol*
(define (lambda$-params expr)
  (list (cadr expr)))

;;; lambda$-body: KExpr -> KExpr
(define (lambda$-body expr)
  (cddr expr))


;;;; Error
;;; error-expr? KExpr -> bool
(define (error-expr? expr)
  (eq? (car expr) 'error))

;;; error-message: KExpr -> KExpr
(define (error-message expr)
  (cadr expr))