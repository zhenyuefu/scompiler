;;;;;;
;;;;;; SCompiler / Syntaxe étendue et expanseur
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE)

#lang racket

(require "utils.scm")

(provide expander)

;;;; On notera SExpr pour le type des expressions Scheme
;;;; (S-expressions)
;;;; On notera ALISTE[alpha beta] comme pour les listes
;;;; d'associations.

;;;;;
;;;;; On rappelle le langage noyau: type KExpr, sous ensemble de SExpr
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


;;;;;
;;;;; Règles et fonctions d'expansion de code

;;;; La conjonction n-aire: and
;;;; Règles d'expansion
; [[ (and e1 e2 ...) ]] ---> (if e1 [[ (and e2 ...) ]] #f)
; [[ (and e) ]] ---> e
; [[ (and) ]] ---> #t

;;; and-expr?: SExpr -> bool
(define (and-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'and)))

;;; and-expr-args: SExpr -> LIST[SExpr]
;;; Précondition: expr est un 'and
(define (and-expr-args expr)
  (cdr expr))

;;; expand-and-args: LIST[SExpr] -> SExpr
(define (expand-and-args args)
  (cond ((null? args) #t)
        ((null? (cdr args)) (car args))
        (else (list 'if (car args)(expand-and-args (cdr args)) #f))))

;;; expand-and: SExpr -> SExpr
;;; Précondition: expr est un 'and
(define (expand-and expr)
  (expand-and-args (and-expr-args expr)))

;;;; La disjonction n-aire: or
;;;; Règles d'expansion
; [[ (or e1 e2 ...) ]] ---> (if e1 e1 [[ (or e2 ...) ]])
; [[ (or e) ]] ---> e
; [[ (or) ]] ---> #f

;;; or-expr?: SExpr -> bool
(define (or-expr? expr)
  (and (pair? expr)
       (eq? 'or (car expr))))

;;; or-expr-args: SExpr -> LIST[SExpr]
;;; Précondition: expr est un 'or
(define (or-expr-args expr)
  (cdr expr))

;;; expand-or-args: LIST[SExpr] -> SExpr
(define (expand-or-args args)
  (cond((null? args) #f)
       ((null? (cdr args)) (car args))
       (else (list 'if (car args) (car args) (expand-or-args (cdr args))))))


;;; expand-or: SExpr -> SExpr
;;; Précondition: expr est un 'or
(define (expand-or expr)
  (expand-or-args (or-expr-args expr)))

; La forme when

;;; when-expr?: SExpr -> bool
(define (when-expr? expr)
  (and (pair? expr)
       (eq? 'when (car expr))))

;;; when-expr-cond: SExpr -> SExpr
(define (when-expr-cond expr)
  (cadr expr))

;;; when-expr-body: SExpr -> LISTE[SExpr]
(define (when-expr-body expr)
  (cddr expr))

;;; expand-when: SExpr -> SExpr
(define (expand-when expr)
  (list 'if (when-expr-cond expr) (cons 'begin (when-expr-body expr)) '(begin)))

;;;; La forme dérivée unless

;;; unless-expr?: SExpr -> bool
(define (unless-expr? expr)
  (and (pair? expr)
       (eq? 'unless (car expr))))

;;; unless-expr-cond: SExpr -> SExpr
(define (unless-expr-cond expr)
  (cadr expr))

;;; unless-expr-body: SExpr -> LISTE[SExpr]
(define (unless-expr-body expr)
  (cddr expr))

;;; expand-unless: SExpr -> SExpr
(define (expand-unless expr)
  (cons 'when (cons (list 'not (unless-expr-cond expr)) (unless-expr-body expr))))

;;;; La forme cond
;;;; Remarque: on force un 'else à la fin
;;;; Règles d'expansion
; [[ (cond (c1 e1 ...) (c2 e2 ...) ... (else ex ...)) ]]
;  ---> (if c1 (begin e1 ...) [[ (cond (c2 e2 ...) ... (else ex ...)) ]]
; [[ (cond (else e ...)) ]] ---> (begin e ...)
;;;; On utilise la transformation auxiliaire sur les listes de clauses
; [[ ( (c1 e1 ...) (c2 e2 ...) ... (else ex ...)) ]]
;  ---> (if c1 (begin e1 ...) [[ ((c2 e2 ...) ... (else ex ...)) ]]

;;; cond-expr?: SExpr -> Bool
(define (cond-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'cond)))

;;; cond-expr-clauses: SExpr -> LIST[LIST[SExpr]]
;;; HYPOTHESE: expr est un 'cond
(define (cond-expr-clauses expr)
  (cdr expr))

;;; clause-condition: LIST[SExpr] -> SExpr
;;; HYPOTHESE: expr est une liste non vide
(define (clause-condition clause)
  (car clause))

;;; clause-exprs: LIST[SExpr] -> LIST[SExpr]
;;; HYPOTHESE: expr est une liste non vide
(define (clause-exprs clause)
  (cdr clause))

;;; build-begin: LIST[LIST[SExpr]] -> SExpr
;;; Précondition: exprs est une liste non-vide
;;; Construit une séquence (begin expr1 expr2 ...)
;;; si exprs contient plus d'une expression, sinon
;;; retourne l'unique expression de exprs.
(define (build-begin exprs)
  (if (pair? (cdr exprs))
      (cons 'begin exprs)
      (car exprs)))

;;; expand-cond-clauses: LIST[LIST[SExpr]] -> SExpr
;;; Transformation d'une liste de clauses de cond
;;; Erreur: si la clause else n'est pas la dernière
;;; Erreur: s'il manque une clause else à la fin
(define (expand-cond-clauses clauses)
  (cond ((null? (cdr clauses)) (if (eq? (clause-condition (car clauses)) 'else)
                                   (build-begin (clause-exprs (car clauses)))
                                   (emit-error "bad cond: must end with else clause")))
        ((eq? 'else (clause-condition (car clauses))) (if (null? (cdr clauses))
                                                          (build-begin (clause-exprs (car clauses)))
                                                          (emit-error "bad cond: else must be last clause")))
        (else (list 'if (clause-condition (car clauses))
                    (build-begin (clause-exprs (car clauses))) (expand-cond-clauses (cdr clauses))))))



;;; expand-cond: SExpr -> SExpr
;;; HYPOTHESE: expr est un 'cond
(define (expand-cond expr)
  (if (null? (cdr expr))
      (list 'begin)
      (expand-cond-clauses (cond-expr-clauses expr))))

;;;; Les formes let*, letrec et let
;;;; Quelques utilitaires

;;; let-bindings: SExpr -> ALIST[Symbol SExpr]
;;; Précondition: expr est un 'let*, un 'letrec ou un 'let
(define (let-bindings expr)
  (cadr expr))

;;; let-exprs: SExpr -> LIST[SExpr]
;;; Précondition: expr est un 'let*, un 'letrec ou un 'let
(define (let-exprs expr)
  (cddr expr))

;;; binding-var: TUPLE[Symbol SExpr] -> Symbol
(define (binding-var b)
  (car b))

;;; binding-val: TUPLE[Symbol SExpr] -> SExpr
(define (binding-val b)
  (cadr b))

;;;; La forme let (parallèle)
;;;; Règle d'expansion
; [[ (let ((v1 e1) ... ) e) ]]
;  ---> ((lambda (v1 ...) e) e1 ...)

;;; let-expr?: SExpr -> Bool
(define (let-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'let)))

;;; expand-let: SExpr -> SExpr
(define (expand-let expr)
  (let ((bindings (let-bindings expr)))
    (cons
     (cons 'lambda
           	   (cons (map binding-var bindings)
                         		 (let-exprs expr)))
     (map binding-val bindings))))


;;;; La forme let* (séquentielle)
;;;; Règles d'expansion
; [[ (let* ((v1 e1) (v2 e2) ...) e ...) ]]
;   ---> (let ((v1 e1)) [[ (let* ((v2 e2) ...) e ...) ]] )
; [[ (let* ((v1 e1)) e ...) ]]
;   ---> (let ((v1 e1)) e ...)
; [[ (let* () e ...) ]] ---> (begin e ...)

;;; let*-expr?: SExpr -> Bool
(define (let*-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'let*)))

;;; expand-let*-loop: ALIST[Symbol SExpr] * LIST[SExpr] -> SExpr
(define (expand-let*-loop bindings exprs)
  (if (pair? bindings)
      (cons 'let
            (cons (list (car bindings))
                  (let ((rest (expand-let*-loop (cdr bindings) exprs)))
                    (if (let-expr? rest)  ;; petite subtilité ici ...
                        (list rest)
                        rest))))
      exprs))

;;; expand-let*: SExpr -> SExpr
;;; Précondition expr est un 'let*
(define (expand-let* exp)
  (expand-let*-loop
   (let-bindings exp)
   (let-exprs exp)))

;;; La forme letrec
;;; Règle d'expanssion
; [[ (letrec ((x1 e1) ...) e ...) ]]
;  ---> (let ((x1 #f) ...) (set! x1 e1) ... e ...)
;;; letrec-expr?: SExpr -> Bool
(define (letrec-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'letrec)))


;;; expand-letrec: SExpr -> SExpr
;;; Précondition: expr est un 'letrec'
(define (expand-letrec expr)
  (let ((bindings (let-bindings expr)))
    (let ((new-bindings
           	   (map (lambda (binding) (list (car binding) #f))
                        		bindings))
          	  (sets (map (lambda (binding) (cons 'set! binding)) bindings)))
      (cons 'let
            	    (cons new-bindings
                          		  (append sets (let-exprs expr)))))))


;;;; Forme define:
;;;;    on restreint à la définition d'un symbole atomique
;;;; NOTA: ne traite pas les définitions récursives
;;;; Règle d'expansion
; [[ (define (f x1 ...) b1 ...) ]]
;  ---> [[ (define f (lambda (x1 ...) b1 ...)) ]]

;;; define-fun-expr?: SExpr -> Bool
(define (define-fun-expr? expr)
  (and (pair? expr)
       (eq? (car expr) 'define)
       (pair? (cadr expr))))

;;; define-fun-header: SExpr -> LIST[Symbol]
;;; Précondition: expr est un 'define
(define (define-fun-header expr)
  (cadr expr))

;;; define-fun-fname: LIST[Symbol] -> Symbol
;;; Précondition: syms est non vide
(define (define-fun-fname syms)
  (car (define-fun-header syms)))

;;; define-fun-params: LIST[Symbol] -> LIST[Symbol]
;;; Précondition: syms est non vide
(define (define-fun-params syms)
  (cdr (define-fun-header syms)))

;;; define-fun-body: SExpr -> LIST[SExpr]
;;; Précondition: expr est un 'define
(define (define-fun-body expr)
  (cddr expr))

;;; define-fun-analyze : SExpr -> TUPLE[LIST[SExpr], LIST[SExpr]]
;;; Précondition : expr est un 'define
(define (define-fun-analyze expr)
  (define (collect exprs defs)
    (if (pair? exprs)
        (if (define-fun-expr? (car exprs))
            (collect (cdr exprs) (cons (car exprs) defs))
            (list (reverse defs) exprs))
        (emit-error "Empty define")))
  (collect (define-fun-body expr) (list)))

;;; expand-fun-letrec : LIST[SExpr] * LIST[SExpr] -> SExpr
;;; Précondition : defs contient au moins une définition interne
;;; Expanse chaque define interne en letrec
(define (expand-fun-letrec defs body)
  (cons 'letrec (cons (map (lambda (def)
                             (list (define-fun-fname def)
                                   (cons 'lambda (cons (define-fun-params def)
                                                       (define-fun-body def))))) defs)
                      body)))


;;; expand-define-fun: SExpr -> SExpr
;;; Précondition: expr est un 'define
(define (expand-define-fun expr)
  (list 'define (define-fun-fname expr)
        (cons 'lambda (cons (define-fun-params expr)
                            (let* ((defs&body (define-fun-analyze expr))
                                   (defs (car defs&body))
                                   (body (cadr defs&body)))
                              (if (pair? defs)
                                  (list (expand-fun-letrec defs body))
                                  body))))))

;;;; La forme named-let
;;;; Règles d'expansion
; [[ (let f ((x1 e1) (x2 e2) ...) e)]]
; ---> (letrec ((f (lambda (x1 x2 ...) e))) (f e1 e2 ...))

;;; namedlet-expr?: SExpr -> Bool
(define (namedlet-expr? expr)
  (and (pair? expr) (eq? (car expr) 'let)
       (pair? (cdr expr)) (symbol? (cadr expr))))

;;; namedlet-name: SExpr -> Symbol
(define (namedlet-name expr)
  (cadr expr))

;;; namedlet-params: SExpr -> ALIST[TUPLE[Symbol SExpr]]
(define (namedlet-params expr)
  (caddr expr))

;;; namedlet-exprs: SExpr -> LIST[SExpr]
(define (namedlet-exprs expr)
  (cdddr expr))

;;; expand-namedlet: SExpr -> SExpr
(define (expand-namedlet expr)
  (list 'letrec (list (list (namedlet-name expr)
                            (cons 'lambda
                                  (cons (map (lambda (params) (car params))
                                             (namedlet-params expr))
                                        (namedlet-exprs expr)))))
        (cons (namedlet-name expr) (map (lambda (params) (cadr params)) (namedlet-params expr)))))

;;;;;
;;; expander: SExpr -> SExpr
(define (expander expr)
  (if (or (not (pair? expr))  (eq? (car expr) 'quote))
      expr                                ; quote ou atome: pas d'expansion
      (if (define-fun-expr? expr) ; cas particuliers pour le define
          (expander (expand-define-fun expr))
          ;; pour le reste
          (let ((eexpr (map expander expr)))  ; expansion des sous-expressions
            (cond ((and-expr? eexpr) (expander (expand-and eexpr)))
                  ((or-expr? eexpr) (expander (expand-or eexpr)))
                  ((cond-expr? eexpr) (expander (expand-cond eexpr)))
                  ((namedlet-expr? eexpr) (expander (expand-namedlet eexpr)))
                  ((let-expr? eexpr) (expander (expand-let eexpr)))
                  ((let*-expr? eexpr) (expander (expand-let* eexpr)))
                  ((letrec-expr? eexpr) (expander (expand-letrec eexpr)))
                  ((define-fun-expr? eexpr) (expander (expand-define-fun eexpr)))
                  ((when-expr? eexpr) (expander (expand-when eexpr)))
                  ((unless-expr? eexpr) (expander (expand-unless eexpr)))
                  (else eexpr))))))

