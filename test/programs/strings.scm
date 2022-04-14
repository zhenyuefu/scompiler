#\A
(= #\A #\B)
(string #\B #\o #\n #\j #\o #\u #\r)
"coucou"
(= "salut" "salut")
(= "salut" "coucou")

;; (define (alloc-a-lot)
;;   (let ((x "salut"))
;;     (define (aux n)
;;       (if (= n 0) x
;;           (let ((y "bonjour"))
;;             (aux (- n 1)))))
;;     (aux 5)))

;; (alloc-a-lot)

(string-length "Salut les amis !")
(string-ref "Coucou" 3)

(define (string->list s)
  (define (aux k l)
    (if (= k (- 1)) l
        (aux (- k 1) (cons (string-ref s k) l))))
  (aux (- (string-length s) 1) (list)))

(string->list "Bonjour")

;; (symbol->string 'Salut)
(number->string 12345)
