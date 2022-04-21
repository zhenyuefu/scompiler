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

(require "libbootstrap.scm")

;;; exportation des fonctions (spécifique à racket)
(provide print
         println
         emit-error
         fetch-index
         string-concat
         base-path
         dir-path
         reroot-path
         append-all
         str)

;;; emit-error: SExpr * ... -> Unit
;;; (emit-error e1 ...) émet un message d'erreur
(define (emit-error args)
  (display "Error: ")
  (for-each display args)
  (newline)
  (error "" "<*-fatal-error-*>" (list)))

;;;;;
;;;;; Utilitaire sur les LIST[alpha]

;;; fetch-index: LIST[alpha] * alpha -> Int+#f
;;; (list-index xs x) donne l'indice de la position de x
;;; dans xs ou #f si x n'est pas dans xs. le premier indice
;;; est 0.
(define (fetch-index xs x)
  (define (fetch ccar ccdr ppair? xs i)
    (if (ppair? xs)
        (if (eq? (ccar xs) x)
            i
            (fetch ccar ccdr ppair? (ccdr xs) (+ i 1)))
        #f))
  ;; corps de fetch-index
  (cond
    ((mpair? xs) (fetch mcar mcdr mpair? xs 0))
    ((pair? xs) (fetch car cdr pair? xs 0))
    ;; sinon
    (else #f)))

;;;;; Utilitaires de chemin

;;; list-split : List[Alpha] -> Alpha -> List[List[Alpha]]
(define (list-split l v)
  (define (aux acc l)
    (if (null? l) (if (null? acc) (list) (list (reverse acc)))
        (if (eq? (car l) v)
            (cons (reverse acc) (aux (list) (cdr l)))
            (aux (cons (car l) acc) (cdr l)))))
  (aux (list) l))

;;; string-split : String * Char -> List[String]
(define (string-split s c)
  (map list->string (list-split (string->list s) c)))

;;; explode-path: String -> LIST[String]
(define (explode-path path)
  (string-split path #\/))

(define (string-concat l sep)
  (define (aux l acc)
    (if (pair? l)
        (aux (cdr l) (string-append (string-append acc sep) (car l)))
        acc))
  (aux (cdr l) (car l)))

;;; combine-path: LIST[String] -> String
(define (combine-path path)
  (if (null? path) "."
      (string-concat path "/")))

;;; base-path: String -> String
(define (base-path path)
  (last (explode-path path)))

;;; dir-path: String -> String
(define (dir-path path)
  (combine-path (drop-right (explode-path path) 1)))

;;; reroot-path: LIST[String] * String -> String
(define (reroot-path path filename)
  (combine-path (list path filename)))

;; append-all : List[List[Alpha]] -> List[Alpha]
(define (append-all ls)
  (if (null? ls) (list)
      (append (car ls) (append-all (cdr ls)))))


(define (str . args)
  (if (pair? args)
      (if (pair? (cdr args))
          (string-append (str (car args)) (apply str (cdr args)))
          (let ((v (car args)))
            (cond ((char? v) (string v))
                  ((number? v) (number->string v))
                  ((string? v) v)
                  (else (let ((outstr (open-output-string)))
                          (write v outstr)
                          (get-output-string outstr))))))
      ""))
