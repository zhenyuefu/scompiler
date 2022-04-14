
#lang racket

(require rackunit
         rackunit/text-ui
	 "../src/expander.scm")

(define expander-tests
  (test-suite 
   "Tests de l'expanseur"
   
   (test-case 
       "Expansion de constantes"
     (check-equal? (expander 42) 42 "Constante entière")
     (check-equal? (expander #t) #t "True")
     (check-equal? (expander #f) #f "False"))

   (test-case
       "Expansion de and"
     (check-equal? (expander '(and (< 2 3) (and 1 2) (= 3 0)))
                   '(if (< 2 3) (if (if 1 2 #f) (= 3 0) #f) #f))

     (check-equal? (expander '(or (< 2 3) (or 1 2) (= 3 0)))
                   '(if (< 2 3) (< 2 3) (if (if 1 1 2) (if 1 1 2) (= 3 0))))
     )

   (test-case
       "Expansion de define"
     (check-equal? (expander '(define (mafonction x) (f 1) (g 2) (h 3)))
                   '(define mafonction (lambda (x) (f 1) (g 2) (h 3))))

     (check-equal? (expander '(define (f x)
                                (define (g x) x) (g x)))
                   '(define f (lambda (x)
                                ((lambda (g) (set! g (lambda (x) x)) (g x)) '<undef>))))

     (check-equal? (expander '(define (f x)
                                (define (g x) x)
                                (define (h x) (+ x 1)) (h (g x))))
                   '(define f (lambda (x)
                                ((lambda (g h)
                                   (set! g (lambda (x) x))
                                   (set! h (lambda (x) (+ x 1))) (h (g x))) '<undef> '<undef>))))

     (check-equal? (expander '(define (f x)
                                (define (g x)
                                  (define (h x) x)
                                  (h x))
                                (g x)))
                   '(define f (lambda (x)
                                ((lambda (g)
                                   (set! g (lambda (x)
                                             ((lambda (h)
                                                (set! h (lambda (x) x))
                                                (h x)) '<undef>)))
                                   (g x)) '<undef>))))
     )

))

(display "Testing : expander.scm\n")
(display "-------\n")
(run-tests expander-tests 'normal)


