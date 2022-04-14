#lang racket

(require "libbootstrap.scm")

(provide read)

(define (char->num c)
  (- (char->integer c) 48))

;;; char-numeric? : Char -> Bool
(define (char-numeric? c)
  (or (eq? #\0 c)
      (eq? #\1 c)
      (eq? #\2 c)
      (eq? #\3 c)
      (eq? #\4 c)
      (eq? #\5 c)
      (eq? #\6 c)
      (eq? #\7 c)
      (eq? #\8 c)
      (eq? #\9 c)))

;;; read-num : FILE -> Int
(define (read-num f)
  (define (aux acc)
    (let ((c (peek-char f)))
      (cond
        ;; Continuing to read the number
        ((char-numeric? c)
         (begin (read-char f)
                (aux (+ (* 10 acc) (char->num c)))))
        ;; Finished parsing the number
        (else acc))))
  (aux 0))

;;; read-string : FILE -> String
(define (read-string f)
  (define (aux acc)
    (let ((c (read-char f)))
      (cond
        ((and (eq? #\\ c) (eq? #\" (peek-char f)))
         (read-char f) (aux (cons #\" acc)))
        ((and (eq? #\\ c) (eq? #\n (peek-char f)))
         (read-char f) (aux (cons #\newline acc)))
        ((eq? #\" c) (list->string (reverse acc)))
        (else (aux (cons c acc))))))
  (read-char f) ;; Skip the first #\"
  (aux (list)))

(define (char-delimiter? c)
  (or (char-whitespace? c)
      (eq? #\( c)
      (eq? #\) c)
      (eq? #\" c)
      (eq? #\' c)
      (eq? #\; c)))

;;; read-symbol : FILE -> Symbol
(define (read-symbol f)
  (define (aux acc)
    (let ((c (peek-char f)))
      (cond
        ((char-delimiter? c) (string->symbol (list->string (reverse acc))))
        (else (begin (read-char f) (aux (cons c acc)))))))
  (aux (list)))

;;; skip-until-newline : FILE -> undefined
;;; Used to skip comments (and #lang directive)
(define (skip-until-newline f)
  (let ((c (read-char f)))
    (unless (eq? #\newline c) (skip-until-newline f))))

;;; peek-string-eq? : FILE -> String -> Bool
(define (peek-string-eq? f s)
  (define (aux n)
    (or (eq? n (string-length s))
        (and (eq? (string-ref s n) (peek-char f n))
             (aux (+ n 1)))))
  (aux 0))

(define (repeat n f)
  (unless (zero? n) (f) (repeat (- n 1) f)))

;;; read-a-char : FILE -> Char
(define (read-a-char f)
  (read-char f) (read-char f) ;; Skip \# and \\
  ;; Special cases : #\space, #\tab and #\newline are accepted
  (cond ((peek-string-eq? f "space") (repeat 5 (lambda () (read-char f))) #\space)
        ((peek-string-eq? f "tab") (repeat 3 (lambda () (read-char f))) #\tab)
        ((peek-string-eq? f "newline") (repeat 7 (lambda () (read-char f))) #\newline)
        (else (read-char f))))

;;; read-bool : FILE -> Bool
(define (read-bool f)
  (read-char f) ;; Skip the first #\#
  (let ((c (read-char f)))
    (cond ((eq? #\t c) #t) ;; Bool #t
          ((eq? #\f c) #f) ;; Bool #f
          (else (error "read-bool")))))

(define (read f)
  (define (aux acc islist)
    (define (cons-or-return v)
      (if islist (aux (cons v acc) islist) v))

    (let ((c (peek-char f)))
      (cond
        ;; End of file.
        ;; The accumulator should be empty (otherwise it means we're missing some parenthesis)
        ((eof-object? c)
         (if (null? acc) c (error "read : eof while expression is not finished")))

        ;; Just continue to read
        ((char-whitespace? c)
         (read-char f) (aux acc islist))

        ;; Entering a comment
        ((eq? #\; c) (skip-until-newline f) (aux acc islist))

        ;; Entering a list
        ((eq? #\( c)
         (read-char f)
         (cons-or-return (aux (list) #t)))

        ;; Exiting a list
        ((and (eq? #\) c) islist)
         (read-char f) (reverse acc))

        ;; Entering a quote
        ((eq? #\' c)
         (read-char f)
         (cons-or-return (list 'quote (aux (list) #f))))

        ;; Reading the #lang directive (just skip)
        ((and (eq? #\# (peek-char f)) (eq? #\l (peek-char f 1)))
         (skip-until-newline f)
         (aux acc islist))

        ;; Reading a characher
        ((and (eq? #\# c) (eq? #\\ (peek-char f 1)))
         (cons-or-return (read-a-char f)))

        ;; Reading a bool
        ((eq? #\# c) (cons-or-return (read-bool f)))

        ;; Reading a number
        ((char-numeric? c) (cons-or-return (read-num f)))

        ;; Reading a string
        ((eq? #\" c) (cons-or-return (read-string f)))

        ;; Reading a symbol
        ((not (char-delimiter? c)) (cons-or-return (read-symbol f)))

        (else (error "read")))))
  (aux (list) #f))
