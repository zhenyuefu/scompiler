;;;;;;
;;;;;; SCompiler / Gestion des primitives
;;;;;;
;;;;;; Sorbonne Universit� -- licence informatique 3�me ann�e
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE)

#lang racket

(provide init-prims
         	 prims-fetch)

;;;; L'environnement des primitives est une simple liste
;;;; de symboles

;; type PrimEnv ==  LIST[Symbol]

;;; init-prims: Unit -> PrimEnv
;;; (init-prims) donne la liste des symboles des
;;; primitives connues.
(define (init-prims)
  '((+ 0 ADD)
    (- 1 SUB)
    (* 2 MUL)
    (/ 3 DIV)
    (= 4 EQ)
    (list 5 LIST)
    (cons 6 CONS)
    (car 7 CAR)
    (cdr 8 CDR)
    (zero? 9 ZEROP)
    (display 10 DISPLAY)
    (newline 11 NEWLINE)
    ))

;;; prims-fetch: PrimEnv * Symbol -> Int + #f
;;; (prims-fetch penv var) retourne la référence de la primitive
;;; var ou #f si la primitive n'est pas connue.
(define (prims-fetch penv var)
  (if (pair? penv)
      (if (eq? (caar penv) var)
          (cadar penv)
          (prims-fetch (cdr penv) var))
      #f))

