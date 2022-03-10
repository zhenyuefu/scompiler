
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

    (check-equal? (expander '(and (> 3 2) (= 4 (+ 2 2)) (not (< 4 4))))
                  '(if (> 3 2)
                       (if (= 4 (+ 2 2))
                           (not (< 4 4))
                           #f)
                       #f))

    (check-equal? (expander '(and (> 2 3) (/ 2 0)))
                  '(if (> 2 3)
                       (/ 2 0)
                       #f))

    (check-equal? (expander '(and (> 3 2)))
                  '(> 3 2))

    (check-equal? (expander '(and))
                  #t)
    
    (check-equal? (expander '(and (< 2 3) (and 1 2) (= 3 0)))
		  '(if (< 2 3) (if (if 1 2 #f) (= 3 0) #f) #f))
    
    )

   (test-case
    "Expansion de cond"
    (check-equal? (expander '(cond
			      ((> 2 3) (+ 2 3))
			      ((= 2 (+ 4 2)) (* 2 6))
			      ((> 3 2) (- 44 2))
			      (else (* 2 3))))
		  '(if (> 2 3) 
		       (+ 2 3)
		       (if (= 2 (+ 4 2)) 
			   (* 2 6)
			   (if (> 3 2) 
			       (begin (display "boum") 
				      (- 44 2))
			       (* 2 3)))))
    
    (check-equal? (expander '(cond))
		  '(begin))

    (check-equal? (expander '(cond ((> 3 2) (+ 2 3))
				   (else (* 3 2))))
		  '(if (> 3 2) 
		       (+ 2 3) 
		       (* 3 2)))

    (check-equal? (expander '(cond ((> 3 2) (- 5 12) (+ 2 3))
				   (else (* 3 2) (+ 44 2))))
		  '(if (> 3 2) 
		       (begin (- 5 12) (+ 2 3))
		       (begin (* 3 2) (+ 44 2))))

    )

   (test-case
    "Expansions de let"

    (check-equal? (expander '(let ((x (+ 4 2))
                                   (y (* 3 9))
                                   (z (- 2 3)))
                               (+ x y z)))
                  '((lambda (x y z)
                      (+ x y z))
                    (+ 4 2) (* 3 9) (- 2 3)))

    (check-equal? (expander '(let ((x (+ 4 2)))
                               (let ((y (+ x 10)))
                                 (let ((z (* x y)))
                                   (+ x y z)))))
                  '((lambda (x)
                      ((lambda (y)
                         ((lambda (z)
                            (+ x y z))
                          (* x y)))
                       (+ x 10)))
                    (+ 4 2)))

    (check-equal? (expander '(let () 33 42))
                  '((lambda () 33 42)))

    )

   (test-case
    "Expansions de let*"

    (check-equal? (expander '(let* ((x (+ 4 2))
                                    (y (+ x 10))
                                    (z (* x y)))
                               (+ x y z)))
                  (expander '(let ((x (+ 4 2)))
                               (let ((y (+ x 10)))
                                 (let ((z (* x y)))
                                   (+ x y z))))))

    )
    
   (test-case
    "Expansions de define"

    (check-equal? (expander '(define (factit n acc)
                               (if (zero? n)
                                   acc
                                   (factit (- n 1) (* n acc)))))
                  '(define factit (lambda (n acc)
                                    (if (zero? n)
                                        acc
                                        (factit (- n 1) (* n acc))))))
    
    (check-equal? (expander '(define (filtermap p? f l)
                               (if (pair? l)
                                   (let ((val (f (car l))))
                                     (if (p? val)
                                         (cons val (filtermap p? f (cdr l)))
                                         (filtermap p? f (cdr l))))
                                   (list))))
                 '(define filtermap (lambda (p? f l)
                                       (if (pair? l)
                                           ((lambda (val)
                                              (if (p? val)
                                                  (cons val (filtermap p? f (cdr l)))
                                                  (filtermap p? f (cdr l))))
                                            (f (car l)))
                                           (list)))))

    (check-equal? (expander '(define (fact n)
                               (define (loop n acc)
                                 (if (zero? n)
                                     acc
                                     (loop (- n 1) (* n acc))))
                               ;; corps
                               (loop n 1)))
                  
                  (expander '(define (fact n)
                               (letrec ((loop (lambda (n acc)
                                                (if (zero? n)
                                                    acc
                                                    (loop (- n 1) (* n acc))))))
                                 (loop n 1)))))

    (check-equal? (expander '(define (f a b)
                               (define (g x y)
                                 (+ a b x y))
                               (define (h e)
                                 (+ (g e b) a))
                               (+ (g a b) (h a))))
                  (expander '(define f (lambda (a b)
                                         (letrec ((g (lambda (x y)
                                                       (+ a b x y)))
                                                  (h (lambda (e)
                                                       (+ (g e b) a))))
                                           (+ (g a b) (h a)))))))
                  
    (check-equal? (expander '(define (f a b)
                               (define (g x y)
                                 (+ a b x y))
                               (define (h e)
                                 (+ (g e b) a))
                               (+ (g a b) (h a))))
		  '(define f
                     (lambda (a b)
                       ((lambda (g h)
                          (set! g (lambda (x y) (+ a b x y)))
                          (set! h (lambda (e) (+ (g e b) a)))
                          (+ (g a b) (h a)))
                        #f
                        #f))))
    )

   (test-case
    "Expansions de letrec"

    (check-equal? (expander '(define (nombre-paire? n)
                               (letrec ((test-paire? (lambda (n)
                                                       (if (zero? n)
                                                           #t
                                                           (test-impaire? (- n 1)))))
                                        (test-impaire? (lambda (n)
                                                         (if (zero? n)
                                                             #f
                                                             (test-paire? (- n 1))))))
                                 (test-paire? n))))
                  (expander '(define (nombre-paire? n)
                               (let ((test-paire? #f)
                                     (test-impaire? #f))
                                 (set! test-paire? (lambda (n)
                                                     (if (zero? n)
                                                         #t
                                                         (test-impaire? (- n 1)))))
                                 (set! test-impaire? (lambda (n)
                                                       (if (zero? n)
                                                           #f
                                                           (test-paire? (- n 1)))))
                                 (test-paire? n)))))
                
   )
   ))

(display "Testing : expander.scm\n")
(display "-------\n")
(run-tests expander-tests 'normal)


