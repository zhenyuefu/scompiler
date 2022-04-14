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

(require "libbootstrap.scm")

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

;;; require-expr?: KExpr -> bool
(define (require-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'require)))

;;; require-path: KExpr -> string
(define (require-path expr)
  (cadr expr))

;;; provide-expr?: KExpr -> bool
(define (provide-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'provide)))
