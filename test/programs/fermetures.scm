(define y 3)
(define add (lambda (x) (+ x y)))

(add 2)
(set! y 4)
(add 2)