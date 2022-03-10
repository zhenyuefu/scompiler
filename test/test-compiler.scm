
#lang racket

(require rackunit
         rackunit/text-ui
	 "../src/compilenoyau.scm")

(define compiler-01-tests
  (test-suite
   "Tests du compilateur (langage noyau, cours 5)"

   ;; ces tests correspondent au cours 5 : compilation du langage noyau, première partie
   
   (test-case 
    "Compilation d'une occurrence de variable"
    (check-equal? (let ((genv (mcons '<undefined> (mcons 'a (mcons 'b (list))))))
		    (compile-expr '() genv '() 'a))
		  '((GFETCH 1)))
    (check-equal? (let ((genv (mcons '<undefined> (mcons 'a (mcons 'b (list))))))
		    (compile-expr '() genv '() 'b))
		  '((GFETCH 2)))
    )

   (test-case
    "Compilation des atomes"
    (check-equal? (compile-expr '() '() '() 42)
		  '((PUSH (INT 42))))

    (check-equal? (compile-expr '() '() '() #t)
		  '((PUSH (BOOL #t))))

    (check-equal? (compile-expr '() '() '() #f)
		  '((PUSH (BOOL #f))))
    
    )

   (test-case
    "Compilation des définitions de variable globales"
    (check-equal? (let* ((genv (mcons '<undefined> (mcons 'a (mcons 'b (list)))))
			 (asm (compile-expr '() genv '() '(define mavar 42))))
		    genv)
		  (mcons '<undefined> (mcons 'a (mcons 'b (mcons 'mavar (list))))))

    (check-equal? (let* ((genv (mcons '<undefined> (mcons 'a (mcons 'begin (list)))))
			 (asm (compile-expr '() genv '() '(define mavar 42))))
		    asm)
		  '((GALLOC) (PUSH (INT 42)) (GSTORE 3) (PUSH (UNIT)))
		   ))

   (test-case
    "Compilation d'un exemple de programme simple mais complet"
    (check-equal? (compile-prog '((define x 42)
				  (define y 34)
				  x
				  y
				  (define z x)
				  z))
		  '((GALLOC)
		    (PUSH (INT 42))
		    (GSTORE 1)
		    (PUSH (UNIT))
		    (POP)
		    (GALLOC)
		    (PUSH (INT 34))
		    (GSTORE 2)
		    (PUSH (UNIT))
		    (POP)
		    (GFETCH 1)
		    (POP)
		    (GFETCH 2)
		    (POP)
		    (GALLOC)
		    (GFETCH 1)
		    (GSTORE 3)
		    (PUSH (UNIT))
		    (POP)
		    (GFETCH 3)
		    (POP))))
   
   ))

(display "Testing : compilenoyau.scm\n")
(display "-------\n")
(run-tests compiler-01-tests 'normal)


