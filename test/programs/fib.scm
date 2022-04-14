(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fibit n a b)
  (if (= n 0)
      a
      (fibit (- n 1) b (+ a b))))


(fib 15)

(fibit 15 0 1)

