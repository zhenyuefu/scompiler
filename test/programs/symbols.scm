'coucou

(define (match-my-symbol x)
  (cond ((= x 'rouge) 1)
        ((= x 'vert) 2)
        ((= x 'jaune) 3)
        (else 0)))

(match-my-symbol 'rouge)
(match-my-symbol 'vert)
(match-my-symbol 'bleu)
