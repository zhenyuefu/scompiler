(define (even n)
  (or (zero? n) (not (odd n))))

(define (odd n)
  (or (= n 1) (even (- n 1))))

(even 12)
(even 13)
