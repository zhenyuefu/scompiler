;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) B.P. under GPLv3.0  (cf. LICENSE) 

#lang racket

;;;; Equalities

;;; eq?: Alpha * Alpha -> Bool
;;; test d'égalité (on n'a qu'une seule forme d'égalité dans notre langage)
(define (eq? x y) (= x y))

;;; equal?: Alpha * Alpha -> Bool
;;; test d'égalité (on n'a qu'une seule forme d'égalité dans notre langage)
(define (equal? x y) (= x y))

;;; for-each: List[Alpha] -> Unit
;;; applique une fonction a une liste de parametres
(define (for-each f l)
  (unless (null? l) (f (car l)) (for-each f (cdr l))))

;;; newline: Unit -> Unit
;;; affiche un retour a la ligne
(define (newline) (display "\n"))

;;; println: Alpha -> Unit
;;; affiche le texte suivi d'un retour à la ligne
(define (println s)
  (display s)
  (newline))

;;;; Paires : les fonctions manquantes

(define (cddr p) (cdr (cdr p)))
(define (cdddr p) (cdr (cddr p)))
(define (cadr p) (car (cdr p)))
(define (caddr p) (car (cddr p)))
(define (cadddr p) (car (cdddr p)))
(define (caar p) (car (car p)))
(define (cadar p) (car (cdr (car p))))

;;;; Listes

;;; length : List[Alpha] -> Int
(define (length l)
  (if (null? l) 0
      (+ 1 (length (cdr l)))))

;;; append : List[Alpha] * List[Alpha] -> List[Alpha]
(define (append l1 l2)
  (if (null? l1) l2
      (cons (car l1) (append (cdr l1) l2))))

;;; flatten : List[Alpha] -> List[Alpha]
(define (flatten l)
  (cond ((pair? l)
         (append (flatten (car l)) (flatten (cdr l))))
        ((null? l) l)
        (else (list l))))

;;; map : (Alpha -> Beta) * List[Alpha] -> List[Beta]
(define (map f l)
  (if (pair? l)
      (cons (f (car l)) (map f (cdr l)))
      (list)))

;;; filter : (Alpha -> bool) * List[Alpha] -> List[Alpha]
(define (filter p? l)
  (if (pair? l)
      (if (p? (car l)) (cons (car l) (filter p? (cdr l)))
          (filter p? (cdr l)))
      (list)))

;;; reverse : List[Alpha] -> List[Alpha]
;;; renverse une liste
(define (reverse l)
  (define (aux l acc)
    (if (null? l) acc
        (aux (cdr l) (cons (car l) acc))))
  (aux l (list)))

;;; last : List[Alpha] -> Alpha
;;; précondition : l ne doit pas ètre vide
(define (last l)
  (if (null? l) (error "input of last shouldnt be empty")
      (if (null? (cdr l)) (car l)
          (last (cdr l)))))

;;; take : List[Alpha] * Int -> List[Alpha]
(define (take l n)
  (cond ((zero? n) (list))
        ((null? l) (error "take : list too short"))
        (else (cons (car l) (take (cdr l) (- n 1))))))

;;; drop : List[Alpha] * Int -> List[Alpha]
(define (drop l n)
  (if (zero? n) l (and (pair? l) (drop (cdr l) (- n 1)))))

;;; drop-right : List[Alpha] * Int -> List[Alpha]
;;; pris dans la bibliothèque standard de racket :)
(define (drop-right l n)
  (define (loop l lead)
    (if (pair? lead)
        (cons (car l) (loop (cdr l) (cdr lead)))
        (list)))
  (loop l (drop l n)))

;;; member : Alpha * List[Alpha] -> Bool
(define (member v l)
  (and (pair? l) (or (= v (car l)) (member v (cdr l)))))

;;; assq : Alpha -> ALIST[Alpha Beta] -> (Alpha * Beta)
(define (assq k l)
  (if (pair? l)
      (if (= k (caar l)) (car l) (assq k (cdr l)))
      #f))

;;; list-prefix? : List[Alpha] -> List[Alpha] -> Bool
(define (list-prefix? l1 l2)
  (or (null? l1)
      (and (eq? (car l1) (car l2)) (list-prefix? (cdr l1) (cdr l2)))))

(define (vector->list l) l)

;;;; Fonctions sur les caractères

(define (char-whitespace? c)
  (or (eq? #\space c)
      (eq? #\newline c)
      (eq? #\tab c)))

(define (char-upper-case? c)
  (let ((i (char->integer c)))
    (and (< 64 i) (< i 91))))

;;;; String : fonctions sur les chaines

;;; string->list : String -> List[Char]
(define (string->list s)
  (define (aux k l)
    (if (= k (- 1)) l
        (aux (- k 1) (cons (string-ref s k) l))))
  (aux (- (string-length s) 1) (list)))

;;; list->string : List[Char] -> String
(define (list->string s)
  (apply string s))

;;; string-append : String * String -> String
(define (string-append s1 s2)
  (list->string (append (string->list s1) (string->list s2))))

;;; string-map : (Char -> Char) * String -> String
(define (string-map f s)
  (list->string (map f (string->list s))))

;;; string-downcase : String -> String
(define (string-downcase s)
  (string-map (lambda (c) (if (char-upper-case? c)
                              (integer->char (+ (char->integer c) 32))
                              c)) s))

;;; string-prefix? : String * String -> Bool
(define (string-prefix? s pref)
  (list-prefix? (string->list pref) (string->list s)))

;;; string-suffix? : String * String -> Bool
(define (string-suffix? s pref)
  (list-prefix? (reverse (string->list pref))
                (reverse (string->list s))))

;;; substring : String * Int * Int -> String
(define (substring s start end)
  (list->string (take (drop (string->list s) start) (- end start))))
