;;;;;;
;;;;;; SCompiler / Sérialisation du bytecode
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0  (cf. LICENSE) 

#lang racket

(require "utils.scm")
(require "bytecode.scm")

(provide serialize-bytecode)

;;;; Résolution des étiquettes
; On calcule une liste d'association ALIST[Symbol Integer]
; qui associe à chaque étiquette sa position dans le fichier
; du bytecode "sérialisé".

;;; BC-codop: BC-instr -> Symbol
(define (BC-codop instr)
  (car instr))

;;; serialized-length : BC-instr -> Integer
(define (serialized-length instr)
  (cond ((BC-PUSH? instr)
	 (if (BC-unit? (BC-PUSH-value instr)) 2 3))
	((BC-LABEL? instr) 0)
	((or (BC-GALLOC? instr) (BC-POP? instr) (BC-RETURN? instr)) 1)
	(else ; JUMP JFALSE GSTORE GFETCH CALL FETCH STORE 
	 2)))

;;; labels-index: LIST[BC-instr] -> ALIST[Symbol Integer]
(define (labels-index instrs)
  (letrec 
      ((index
	(lambda (instrs lnum)
	  (if (pair? instrs)
	      (let ((instr (car instrs)))
		(if (BC-LABEL? instr)
		    (cons (list (BC-LABEL-label instr) lnum)
			  (index (cdr instrs) lnum))
		    (index (cdr instrs) (+ lnum (serialized-length instr)))))
	      (list)))))
    (index instrs 0)))

;;; get-label-index: Symbol * ALIST[Symbol Integer] -> Integer
(define (get-label-index lb labels)
  (cadr (assq lb labels)))

;;; BCValue-type: BCValue -> Symbol
(define (BCValue-type v)
  (car v))

;;; serialize-type: BCValue -> Integer
(define (serialize-type v)
  (cadr (assq (BCValue-type v) type-codes)))

;;; serialize-constant: BCValue -> Integer
(define (serialize-constant v labels)
  (cond ((BC-bool? v)
	 (if (BC-bool-val v) 1 0))
	((BC-fun? v)
	 (get-label-index (BC-fun-label v) labels))
	(else (cadr v))))

;;; serialize-value: BCValue * LIST[Symbol] -> LIST[Int]
(define (serialize-value v labels)
  (cons (serialize-type v)
        (if (BC-unit? v)
            (list)
            ;; pas Unit
            (list (serialize-constant v labels)))))

;;; serialize-instr: BC-instr * LIST[Symbol] -> LIST[Int]
(define (serialize-instr instr labels)
  (if (BC-LABEL? instr)
      (list)
      ;; pas un label
      (let ((op (cadr (assq (car instr) op-codes))))
        (cons op
              (cond ((BC-GALLOC? instr) (list))
                    ((BC-RETURN? instr) (list))
                    ((BC-POP? instr) (list))
                    ((BC-PUSH? instr)  (serialize-value (BC-PUSH-value instr) labels))
                    ((BC-GSTORE? instr) (list (BC-GSTORE-ref instr)))
                    ((BC-JUMP? instr)   (list (get-label-index (BC-JUMP-label instr) labels)))
                    ((BC-GFETCH? instr) (list (BC-GFETCH-ref instr)))
                    ((BC-CALL? instr)   (list (BC-CALL-nbarg instr)))
                    ((BC-FETCH? instr)  (list (BC-FETCH-ref instr)))
                    ((BC-JFALSE? instr) (list (get-label-index (BC-JFALSE-label instr) labels)))
                    ((BC-STORE? instr)  (list (BC-STORE-ref instr)))
                    (else (emit-error "can't serialize: " instr)))))))

;;; serialize-instrs: LIST[BC-instr] * labels -> LIST[Int]
(define (serialize-instrs instrs labels)
  (if (pair? instrs)
      (append (serialize-instr (car instrs) labels)
              (serialize-instrs (cdr instrs) labels))
      (list)))
  
;;; serialize: LIST[BC-instr] -> LIST[Int]
(define (serialize-bytecode bc)
  (let ((labels (labels-index bc)))
    (serialize-instrs bc labels)))

