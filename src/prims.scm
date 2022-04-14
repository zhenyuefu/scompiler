;;;;;;
;;;;;; SCompiler / Gestion des primitives
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE) 

#lang racket

(require "libbootstrap.scm")

(provide init-prims
	 prims-fetch)

;;;; L'environnement des primitives est une simple liste
;;;; de symboles

;; type PrimEnv ==  LIST[Symbol]

;;; init-prims: Unit -> PrimEnv
;;; (init-prims) donne la liste des symboles des
;;; primitives connues.
(define (init-prims)
  '((= 0 EQ)
    ;; Integer primitives
    (+ 1 ADD)
    (- 2 SUB)
    (* 3 MUL)
    (/ 4 DIV)
    (< 5 LT)
    (zero? 6 ZEROP)
    (integer? 7 INTEGERP)
    ;; List primitives
    (list 10 LIST)
    (cons 11 CONS) (mcons 11 CONS)
    (car 12 CAR) (mcar 12 CAR)
    (cdr 13 CDR) (mcdr 13 CDR)
    (null? 14 NULLP)
    (pair? 15 PAIRP) (mpair? 15 PAIRP)
    (set-car! 16 SETCAR) (set-mcar! 16 SETCAR)
    (set-cdr! 17 SETCDR) (set-mcdr! 17 SETCDR)
    ;; Char primitives
    (char->integer 20 CHARTOINTEGER)
    (integer->char 21 INTEGERTOCHAR)
    (char? 22 CHARP)
    ;; String primitives
    (string 30 STRING)
    (string-ref 31 STRINGREF)
    (string-length 32 STRINGLENGTH)
    (number->string 33 NUMBERTOSTRING)
    (string? 34 STRINGP)
    ;; Symbols TODO
    ;; Read/Write primitives
    (display 50 DISPLAY)
    (open-input-file 51 OPENINPUTFILE)
    (open-output-file 52 OPENOUTPUTFILE)
    (close-input-port 53 CLOSEINPUTPORT)
    (close-output-port 54 CLOSEOUTPUTPORT)
    (read-char 55 READCHAR)
    (peek-char 56 PEEKCHAR)
    (eof-object? 57 EOFP)
    ;; Misc
    (apply 60 APPLY)
    (error 61 ERROR)
    (current-command-line-arguments 62 CURRENTCMDARGS)
    (exit 63 EXIT)))

;;; prims-fetch: PrimEnv * Symbol -> Int + #f
;;; (prims-fetch penv var) retourne la référence de la primitive
;;; var ou #f si la primitive n'est pas connue.
(define (prims-fetch penv var)
  (if (pair? penv)
      (if (eq? (caar penv) var)
          (cadar penv)
          (prims-fetch (cdr penv) var))
      #f))
