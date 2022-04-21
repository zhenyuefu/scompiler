#lang racket

(require "utils.scm")

(provide lexing
         lex-error?
         read-num
         read-identifier)

(define (mk-lexer input)
  (let* ((input input)
         (pos 0)
         (cpos 1)
         (lpos 1)
         (is-eof? (lambda (n) (= (+ pos n) (string-length input))))
         (do-peek (lambda (n) (if (is-eof? n)
                                  #f
                                  (string-ref input (+ pos n)))))
         (do-next (lambda () (let ((ch (do-peek 0)))
                               (cond ((equal? ch #f) ch)
                                     ((char=? ch #\newline)
                                      (set! lpos (+ lpos 1))
                                      (set! cpos 1)
                                      (set! pos (+ pos 1))
                                      ch)
                                     (else (set! cpos (+ cpos 1))
                                           (set! pos (+ pos 1))
                                           ch))))))
    (lambda (msg)
      (case msg
        ((eof?) (is-eof? 0))
        ((linepos) lpos)
        ((colpos) cpos)
        ((peek) (do-peek 0))
        ((peekn) (lambda (n) (do-peek n)))
        ((next!) (do-next))
        ((show) (substring input pos))
        (else 'does-not-understand)))))


(define (lex-error? v)
  (and (pair? v)
       (eq? (car v) 'lex-error)))

(define (digit->int ch)
  (- (char->integer ch)
     (char->integer #\0)))

(define (read-char lexer ch)
  (let ((rch (lexer 'peek)))
    (if (equal? rch ch)
        (lexer 'next!)
        #f)))

(define (read-char-range lexer cmin cmax)
  (let ((ch (lexer 'peek)))
    (cond ((not ch) #f)
          ((and (char>=? ch cmin) (char<=? ch cmax))
           (lexer 'next!))
          (else #f))))

(define (read-char-pred lexer pred?)
  (let ((ch (lexer 'peek)))
    (cond ((not ch) #f)
          ((pred? ch) (lexer 'next!))
          (else #f))))

(define (skip-spaces lexer)
  (let ((ch (lexer 'peek)))
    (when (and (char? ch) (char-whitespace? ch))
      (lexer 'next!)
      (skip-spaces lexer))))

(define (read-digit1 lexer)
  (read-char-range lexer #\1 #\9))

(define (read-digit lexer)
  (read-char-range lexer #\0 #\9))

(define test-input1 (mk-lexer "42"))

(read-digit1 test-input1)
;; => 4
(read-digit1 test-input1)
;; => 2
(read-digit1 test-input1)
;; => #f

(define test-input2 (mk-lexer "20"))

(read-digit1 test-input2)
;; => 2
(read-digit1 test-input2)
;; => #f
(read-digit test-input2)
;; => 0

(define test-input3 (mk-lexer "  \n   42"))
(skip-spaces test-input3)
(test-input3 'show)
;; => "42"
(test-input3 'linepos)
;; => 2

(define (read-digits lexer val)
  (let ((res (read-digit lexer)))
    (if (char? res)
        (read-digits lexer (str val res))
        val)))

(define (read-pos lexer)
  (define (read-first)
    (read-digit1 lexer))
  ;; body
  (let ((res (read-first)))
    (if (char? res)
        (read-digits lexer (string res))
        res)))

(define (read-nat lexer)
  (let ((x0 (read-char lexer #\0)))
    (if (char? x0)
        (let ((x1 (read-digit lexer)))
          (if (char? x1) #f "0"))
        (read-pos lexer))))

(define (read-int lexer)
  (let ((x0 (read-char lexer #\-)))
    (if (char? x0)
        (let ((x1 (read-pos lexer)))
          (if x1 (str x0 x1) #f))
        (read-nat lexer))))

(define (read-decimal lexer)
  (let ((x0 (read-char lexer #\.)))
    (if (char? x0)
        (let ((x1 (read-digits lexer "")))
          (if x1 (str x0 x1) #f))
        "")))

(define (read-exponent lexer)
  (let ((x0 (read-char lexer #\e)))
    (if (char? x0)
        (let ((x1 (read-int lexer)))
          (if x1 (str x0 x1) #f))
        "")))

(define (read-num lexer)
  (let ((x0 (read-int lexer)))
    (if x0
        (let ((x1 (read-decimal lexer)))
          (if x1
              (let ((x2 (read-exponent lexer)))
                (if x2
                    (string->number (str x0 x1 x2))
                    #f))
              #f))
        #f)))

(read-num (mk-lexer "42")) ;; => 42
(read-num (mk-lexer "00004"))
(read-num (mk-lexer "0"))
(read-num (mk-lexer "-34"))
(read-num (mk-lexer "-0"))
(read-num (mk-lexer "8."))
(read-num (mk-lexer "8.235"))
(read-num (mk-lexer "8e3"))
(read-num (mk-lexer "8.1e-3"))

(define (id-char? ch)
  (or (and (char>=? ch #\a) (char<=? ch #\z))
      (and (char>=? ch #\A) (char<=? ch #\Z))
      (and (char>=? ch #\0) (char<=? ch #\9))
      (char=? ch #\_) (char=? ch #\=)
      (char=? ch #\*) (char=? ch #\-) (char=? ch #\+) (char=? ch #\/)))

(define (id-char-first? ch)
  (and (id-char? ch)
       (not (and (char>=? ch #\0) (char<=? ch #\9)))))

(define (read-identifier lexer)
  (define (read-first)
    (read-char-pred lexer id-char-first?))
  (define (read-rest prefix)
    (let ((res (read-char-pred lexer id-char?)))
      (if (char? res)
          (read-rest (string-append prefix (string res)))
          (string->symbol prefix))))
  ;; body
  (let ((res (read-first)))
    (if (char? res)
        (read-rest (string res))
        res)))

(define test-input4 (mk-lexer "function fact("))
(read-identifier test-input4)
(skip-spaces test-input4)
(read-identifier test-input4)
(test-input4 'show)

(define (kw? s)
  (or (eq? s 'function)
      (eq? s 'if)
      (eq? s 'then)
      (eq? s 'else)
      (eq? s 'return)
      (eq? s '==)
      (eq? s '*)
      (eq? s '+)))


(define *punctuation* '((#\( lparen)
                        (#\) rparen)
                        (#\{ lcurly)
                        (#\} rcurly)
                        (#\; semi)
                        (#\, colon)))

(define (read-punct lexer)
  (let ((ch (lexer 'peek)))
    (let ((res (assoc ch *punctuation*)))
      (if res
          (begin (lexer 'next!)
                 (cadr res))
          #f))))

(define (read-bool lexer)
  (let ((ch1 (lexer 'peek)))
    (if (equal? ch1 #\#)
        (let* ((ch2 ((lexer 'peekn) 1))
               (res (cond ((equal? ch2 #\t) 'true)
                          ((equal? ch2 #\f) 'false) ; remarque : on ne peut pas retourner #f
                          (else #f))))
          (if res
              (begin (lexer 'next!)
                     (lexer 'next!)
                     res)
              res))
        #f)))


(define (lex-error lexer)
  (let* ((info (if (lexer 'eof?)
                   "unexpected end of file"
                   (str "unexpected character '" (lexer 'peek) "'")))
         (errmsg (string-append "Lexical Error: line " (str (lexer 'linepos)) " col " (str (lexer 'colpos))
                                "\n .. " info)))
    (list 'lex-error errmsg)))

(define (next-token lexer)
  (skip-spaces lexer)
  (if (lexer 'eof?)
      'eof
      (let ((num (read-num lexer)))
        (if (number? num)
            (list 'num num)
            (let ((ident (read-identifier lexer)))
              (if (symbol? ident)
                  (if (kw? ident)
                      ident
                      (list 'sym ident))
                  (let ((punct (read-punct lexer)))
                    (if (symbol? punct)
                        punct
                        (lex-error lexer)))))))))

(define (lexing filename input)
  (define (loop lexer res)
    (let ((tok (next-token lexer)))
      ;(display (str "tok = " tok)) (newline)
      (cond  ((equal? tok 'eof) (reverse res))
             ((lex-error? tok) tok)
             (else (loop lexer (cons tok res))))))
  (let ((lexer (mk-lexer input)))
    (loop lexer (list))))

(lexing "<string>" "function fact")

(lexing "<string>" "function ç fact")

(define fact-ex
  "function fact(n) {
     if (n == 0) {
       return 1;
     } else {
      return n * fact(n - 1);
     }
   }

  fact(5);
  }
")

(lexing "<string>" fact-ex)

(define identity-ex
  "function identity(x) {
  return x;
}

identity(42);")

(lexing "<string>" identity-ex)