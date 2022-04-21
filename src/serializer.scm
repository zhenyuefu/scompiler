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

;;;; Symbol table
; type SymbolTable = LIST[Symbol Integer]

;;; build-symbol-table: LIST[BCInstr] -> SymbolTable
;;; (build-symbol-table bc) collects the symbols appearing in bc
(define (build-symbol-table instrs)
  (define (aux instrs table n)
    (if (pair? instrs)
        (if (and (BC-PUSH? (car instrs))
                 (BC-symbol? (BC-PUSH-value (car instrs))))
            (let ((s (BC-symbol-val (BC-PUSH-value (car instrs)))))
              (if (assq s table) (aux (cdr instrs) table n)
                  (aux (cdr instrs) (cons (list s n) table) (+ n 1)))
              )
            (aux (cdr instrs) table n))
        table)
    )
  (reverse (aux instrs '() 0)))

;;;; Serialization

;;; get-label-index: Symbol * ALIST[Symbol Integer] -> Integer
(define (get-label-index lb labels)
  (cadr (assq lb labels)))

;;; BCValue-type: BCValue -> Symbol
(define (BCValue-type v)
  (car v))

;;; serialize-type: BCValue -> Integer
(define (serialize-type v)
  (cadr (assq (BCValue-type v) type-codes)))

;;; serialize-constant: BCValue * LIST[Symbol] * SymbolTable -> Integer
(define (serialize-constant v labels symtable)
  (cond
    ((BC-bool? v)
     (if (BC-bool-val v) 1 0))
    ((BC-char? v)
     (char->integer (BC-char-val v)))
    ((BC-fun? v)
     (get-label-index (BC-fun-label v) labels))
    ((BC-symbol? v)
     (cadr (assq (BC-symbol-val v) symtable)))
	(else (cadr v))))

;;; serialize-value: BCValue * LIST[Symbol] * SymbolTable -> LIST[Int]
(define (serialize-value v labels symtable)
  (cons (serialize-type v)
        (if (BC-unit? v)
            (list)
            ;; pas Unit
            (list (serialize-constant v labels symtable)))))

;;; serialize-instr: BC-instr * LIST[Symbol] * SymbolTable -> LIST[Int]
(define (serialize-instr instr labels symtable)
  (if (BC-LABEL? instr)
      (list)
      ;; pas un label
      (let ((op (cadr (assq (car instr) op-codes))))
        (cons op
              (cond ((BC-GALLOC? instr) (list))
                    ((BC-RETURN? instr) (list))
                    ((BC-POP? instr) (list))
                    ((BC-PUSH? instr)  (serialize-value (BC-PUSH-value instr) labels symtable))
                    ((BC-GSTORE? instr) (list (BC-GSTORE-ref instr)))
                    ((BC-JUMP? instr)   (list (get-label-index (BC-JUMP-label instr) labels)))
                    ((BC-GFETCH? instr) (list (BC-GFETCH-ref instr)))
                    ((BC-CALL? instr)   (list (BC-CALL-nbarg instr)))
                    ((BC-FETCH? instr)  (list (BC-FETCH-ref instr)))
                    ((BC-JFALSE? instr) (list (get-label-index (BC-JFALSE-label instr) labels)))
                    ((BC-STORE? instr)  (list (BC-STORE-ref instr)))
                    (else (emit-error (list "can't serialize: " instr))))))))

;;; serialize-instrs: LIST[BC-instr] * labels * SymbolTable -> LIST[Int]
(define (serialize-instrs instrs labels symtable)
  (if (pair? instrs)
      (append (serialize-instr (car instrs) labels symtable)
              (serialize-instrs (cdr instrs) labels symtable))
      (list)))
  
;;; serialize: LIST[BC-instr] -> SymbolTable * LIST[Int]
(define (serialize-bytecode bc)
  (let ((labels (labels-index bc))
        (symtable (build-symbol-table bc)))
    (list symtable (serialize-instrs bc labels symtable))))

