;;;;;;
;;;;;; SCompiler / Compilation du langage noyau
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE) 

#lang racket

(require "libbootstrap.scm")
(require "utils.scm")
(require "syntax.scm")
(require "prims.scm")
(require "bytecode.scm")

(provide
 compile-prog
 ;; les exports suivants ne sont pas nécessaires, mais utiles
 ;; pour tester ...
 compile-expr
 fetch-index
 compile-var
 compile-atom
 compile-define
 genv-extend!
 compile-seq
 )

;;;;;; Types de base empruntés au langage Scheme
; SExpr : type de toutes les expressions Scheme
; Procedure : type des procédures Scheme
; Unit : type des "non valeurs" (effets de bords)
; bool : type des booléens Scheme
; LIST[alpha] : type des listes homogènes
; ALIST[alpha beta] : type des listes d'association où
;                     alpha est le type des clés et
;                     beta le type des valeurs

;;;;; NOTA: le type abstrait BC-instr pour le code octet
; est défini dans le fichier bytecode.scm; le type abstrait
; pour KExpr est défini dans le fichier syntax.scm; les
; utilitaires d'affichage et d'erreur sont dans utils.scm

;;;;;
;;;;; Les environnements
; Les environnements manipulés par le compilateur sont
; des listes de symboles. Il y en a trois sortes.


;;;; Environnement des variables globales
; type GlobEnv == LIST[Symbol]

;;; GUndefined: Symbol
;;; Nom protégé '<undefined>
(define GUndefined '<undefined>)

;;; init-genv: Unit -> GlobEnv
;;; (init-genv) retourne un environnement global vide:
;;; seule le nom protégé GUdefined est
;;; présent.
(define (init-genv)
  (mcons GUndefined (list)))

;;; genv-fetch: GlobEnv * Symbol -> Int + #f
;;; (genv-fetch genv var) retourne la référence de la variable 
;;; globale var dans l'environnement global genv ou #f si la 
;;; variable n'est pas connue.
(define (genv-fetch genv var)
  (fetch-index genv var))

;;; genv-extend!: GlobEnv * Symbol -> Int
;;; (genv-extend! genv var) alloue une nouvelle place dans 
;;; l'environnement global genv pour la variable var et
;;; retourne son indice.
;;; ERREUR si la variable porte le nom protégé <#undefined>
;;; ERREUR si la variable est déjà allouée (redéfinition interdite)
(define (genv-extend! genv var)
  (define (extend! genv gref)
    (if (eq? (mcar genv) var)
        (emit-error (list "Variable " var " already defined"))
        (if (mpair? (mcdr genv))
            (extend! (mcdr genv) (+ gref 1))
            (begin (set-mcdr! genv (mcons var (list)))
                   (+ gref 1)))))
  ;; body
  (if (eq? var GUndefined)
      (emit-error (list "Variable " var " is protected"))
      (extend! genv 0)))

;;; collect-genv : LIST[SExpr] -> GlobEnv
;;; Rècupère toutes les définitions globales d'un programme pour crée en genv
(define (collect-genv prog)
  (define (aux ge prog)
    (unless (null? prog)
      (when (and (pair? (car prog)) (define-expr? (car prog)))
        (genv-extend! ge (define-var (car prog))))
      (aux ge (cdr prog))))
  (let ((ge (init-genv)))
    (aux ge prog)
    ge))

;;;; Environnement lexical
; type LexEnv == LIST[Symbol]

;;; env-fetch: LexEnv * Symbol -> Int + #f
;;; (env-fetch lenv var) retourne la référence d'une variable var
;;; dans l'environnement lexical lenv ou #f si la variable n'est 
;;; pas connue.
(define (env-fetch lenv var)
  (fetch-index lenv var))
 
;;;; Gestion des symboles d'étiquettes
; Une étiquette est un symbole constitué de la lettre 'L' 
; suivie d'un nombre positif (son rang).

;;; crée le rang 0 des étiquettes.
(define *label* 0)

;;; reset-label!: Unit -> Unit
;;; (reset-label!) remet à 0 le rang des étiquettes.
(define (reset-label!)
  (set! *label* 0))

;;; next-label: Unit -> Symbol
;;; (next-label!) donne un nouveau symbole d'étiquette.
(define (next-label!)
  (let ((lbl (string->symbol 
	      (string-append "L" (number->string *label*)))))
    (set! *label* (+ *label* 1))
    lbl))

;;;; Compilation d'une expression (langage noyau)

;;; compile-expr: 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
;;; (compile-expr prims genv env expr) donne la liste des 
;;; instructions pour l'exécution de l'expression expr. Avec
;;;  prims : liste des primitives disponibles
;;;  genv : environnement global
;;;  env : environnement lexical
;;;  expr : expression à compiler
(define (compile-expr prims genv env expr)
  (cond ((symbol? expr) (compile-var prims genv env expr))
        ((atom? expr) (compile-atom prims genv env expr))
        ((quote-expr? expr) (compile-quote prims genv env expr))
        ((if-expr? expr) (compile-if prims genv env expr))
        ((set!-expr? expr) (compile-set! prims genv env expr))
        ((begin-expr? expr) (compile-begin prims genv env expr))
        ((define-expr? expr) (compile-define prims genv env expr))
        ((lambda-expr? expr) (compile-lambda prims genv env expr))
        (else (compile-apply prims genv env expr))))       

;;;; Symboles
;;; compile-var: 
;;;   PrimEnv * GlobEnv * LexEnv * Symbol -> LIST[BCInstr]
(define (compile-var prims genv env expr)
  ;; Recherche en priorité dans l'environnement lexical
  ;; (variables locales prioritaires)
  (let ((ref (env-fetch env expr)))
    (if ref
	(list (BC-FETCH ref))
	;; Sinon, recherche dans l'environnement global
	(let ((gref (genv-fetch genv expr)))
	  (if gref
	      (list (BC-GFETCH gref))
	      ;; Sinon, recherche de primitive
	      (let ((pref (prims-fetch prims expr)))
		(if pref
		    (list (BC-PUSH (BC-prim pref)))
		    ;; Sinon ... symbole inconnu
		    (emit-error (list "Not in scope: " expr)))))))))

;;;; Atomes (valeurs immédiates)

;;; compile-string:
;;;   String -> LIST[BCInstr]
(define (compile-string prims str)
  (define (aux l ins)
    (if (pair? l)
        (aux (cdr l) (cons (BC-PUSH (BC-char (car l))) ins))
        ins))
  (aux (string->list str)
       (list (BC-PUSH (BC-prim (prims-fetch prims 'string)))
             (BC-CALL (string-length str)))))

;;; compile-atom: 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-atom prims genv env expr)
  (cond ((integer? expr) (list (BC-PUSH (BC-int expr))))
	((eq? expr #t) (list (BC-PUSH (BC-bool #t))))
	((eq? expr #f) (list (BC-PUSH (BC-bool #f))))
  ((char? expr) (list (BC-PUSH (BC-char expr))))
  ((string? expr) (compile-string prims expr))
	(else (emit-error (list "Unsupported atom: " expr)))))

;;;; Citations
;;; construct: SExpr -> SExpr
;;; (construct L) avec L=(x1 .. xn), construit
;;; l'expression équivalente à
;;;    (cons 'x1 (cons ... (cons 'xn (list))))
(define (construct L)
  (if (pair? L)
      (list 'cons (list 'quote (car L)) (construct (cdr L)))
      (quote (list))))

;;; compile-quote: 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-quote prims genv env expr)
  (cond
   ((atom? (cadr expr))
    (compile-atom prims genv env (cadr expr)))
   ((pair? (cadr expr))
    (compile-expr prims genv env (construct (cadr expr))))
   (else (emit-error (list "Unsupported quote: " expr)))))

;;;; Alternative
;;; compil-if: PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-if prims genv env expr)
  (let ((onfalse (next-label!))
        (contlbl (next-label!)))
    (append-all (list (compile-expr prims genv env (if-cond expr))
                      (list (BC-JFALSE onfalse))
                      (compile-expr prims genv env (if-then expr))
                      (list (BC-JUMP contlbl))
                      (list (BC-LABEL onfalse))
                      (compile-expr prims genv env (if-else expr))
                      (list (BC-LABEL contlbl))))))

;;;; Affectation
;;; compile-set!: 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-set! prims genv env expr)
  (let ((sym (set!-sym expr)))
    (append-all (list (compile-expr prims genv env (set!-expr expr))
                      (let ((ref (env-fetch env sym)))
                        (if ref
                            (list (BC-STORE ref))
                            (let ((gref (genv-fetch genv sym)))
                              (if gref
                                  (list (BC-GSTORE gref))
                                  (emit-error (list "Undefined variable: " sym))))))
                      (list (BC-PUSH (BC-unit)))))))

;;;; Définition
;;; compile-define : 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-define prims genv env expr)
  (let ((gref (genv-fetch genv (define-var expr))))
    (append-all (list (list (BC-GALLOC))
                      (compile-expr prims genv env (define-expr expr))
                      (list (BC-GSTORE gref))
                      (list (BC-PUSH (BC-unit)))))))

;;;; Séquence
;;; compile-begin : 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-begin prims genv env expr)
  (if (null? (begin-body expr)) (list (BC-PUSH (BC-unit)))
      (compile-seq prims genv env (begin-body expr))))

;;; compile-seq : 
;;;   PrimEnv * GlobEnv * LexEnv * LIST[KExpr] -> LIST[BCInstr]
(define (compile-seq prims genv env exprs)
  (if (pair? exprs)
      (if (pair? (cdr exprs))
          (append-all (list (compile-expr prims genv env (car exprs))
                            (list (BC-POP)) ;; cleanup the stack
                            (compile-seq prims genv env (cdr exprs))))
          ;; last
          (compile-expr prims genv env (car exprs)))
      (emit-error (list "Cannot compile: empty sequence"))))

;;;; Expression fonctionnelle
;;; compile-lambda : 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-lambda prims genv env expr)
  ;;(display-alln "compile-lambda: expr=" expr)
  (let ((flbl (next-label!)) ;; function label
        (clbl (next-label!)) ;; continuation label
        (nenv (append (lambda-params expr) env)))
    (append-all (list (list (BC-JUMP clbl))
                      (list (BC-LABEL flbl))
                      (compile-seq prims genv nenv (lambda-body expr))
                      (list (BC-RETURN))
                      (list (BC-LABEL clbl))
                      (list (BC-PUSH (BC-fun flbl)))))))

;;;; Application
;;; compile-apply : 
;;;   PrimEnv * GlobEnv * LexEnv * KExpr -> LIST[BCInstr]
(define (compile-apply prims genv env expr)
  (append-all (list (compile-args prims genv env (reverse (cdr expr)))
                    (compile-expr prims genv env (car expr))
                    (list (BC-CALL (length (cdr expr)))))))

;;; compile-args : 
;;;   PrimEnv * GlobEnv * LexEnv * LIST[KExpr] -> LIST[BCInstr]
(define (compile-args prims genv env args)
  (if (pair? args)
      (append (compile-expr prims genv env (car args))
              (compile-args prims genv env (cdr args)))
      (list)))

;;;;;
;;;;; Fonction principale
;;;;;

;;; compile-prog: KExpr -> LIST[BCInstr]
(define (compile-prog prog)
  (reset-label!)
  (append
   (compile-seq (init-prims) (collect-genv prog) (list) prog)
   (list (BC-POP)))) ;; clean-up

