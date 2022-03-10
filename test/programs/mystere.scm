(define mystere (lambda (x) (if (= x 0) 1 (* x (mystere (- x 1))))))
(mystere 3)