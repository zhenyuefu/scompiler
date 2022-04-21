#lang racket

(require "utils.scm")
(require "lexing.scm")

(define (mk-parser tokens)
  (let ((tokens tokens))
    (lambda (msg)
      (case msg
        ((eof?) (null? tokens))
        ((peek) (if (null? tokens)
                    #f
                    (car tokens)))
        ((next!) (let ((tok (car tokens)))
                   (set! tokens (cdr tokens))
                   tok))
        ((fetch) tokens)
        (else 'doest-not-understand)))))

(define (parse-error msg)
  (list 'parse-error msg))

(define (parse-error? v)
  (and (pair? v)
       (eq? (car v) 'parse-error)))

(define (parse-prog parser)
  (define (loop instrs)
    (if (parser 'eof?)
        (reverse instrs)
        (let ((fdef (parse-fundef parser)))
          (if (parse-error? fdef)
              (let ((instr (parse-instr parser)))
                (if (parse-error? instr)
                    (parse-error "expecting a function definition or an instruction")
                    (let ((res (parse-literal parser 'semi)))
                      (if (parse-error? res)
                          res
                          (loop (cons instr instrs))))))
              ;; function def
              (loop (cons fdef instrs))))))
  ;; body
  (loop (list)))


(define (parse-literal parser lit)
  (let ((tok (parser 'peek)))
    (if (eq? tok lit)
        (begin (parser 'next!)
               tok)
        (parse-error (str "expecting '" lit "', got '" tok "'")))))

(define (parse-num parser)
  (let ((tok (parser 'peek)))
    (if (and (pair? tok)
             (eq? (car tok) 'num))
        (begin (parser 'next!)
               (cadr tok))
        (parse-error (str "expecting a number, got '" tok "'")))))

(define (parse-ident parser)
  (let ((tok (parser 'peek)))
    (if (and (pair? tok)
             (eq? (car tok) 'sym))
        (begin (parser 'next!)
               (cadr tok))
        (parse-error (str "expecting an identifier, got '" tok "'")))))

(define (parse-fundef parser)
  (let ((res (parse-literal parser 'function)))
    (if (parse-error? res)
        res
        (let ((fname (parse-ident parser)))
          (if (parse-error? fname)
              fname
              (let ((res (parse-literal parser 'lparen)))
                (if (parse-error? res)
                    res
                    (let ((param (parse-ident parser)))
                      (if (parse-error? param)
                          param
                          (let ((res (parse-literal parser 'rparen)))
                            (if (parse-error? res)
                                res
                                (let ((body (parse-block parser)))
                                  (if (parse-error? body)
                                      body
                                      (cons 'function (cons (list fname param) body)))))))))))))))

(define (parse-block parser)
  (let ((res (parse-literal parser 'lcurly)))
    (if (parse-error? res)
        res
        (let ((instr (parse-instr parser)))
          (if (parse-error? instr)
              instr
              (let ((res (parse-literal parser 'semi)))
                (if (parse-error? res)
                    res
                    (let ((res (parse-literal parser 'rcurly)))
                      (if (parse-error? res)
                          res
                          (list instr))))))))))

;;; Monadic form
(define (parse-map parser . funs)
  (define (aux funs)
    (if (pair? funs)
        (let ((p1 ((car funs) parser)))
          (if (parse-error? p1) p1
              (let ((p2 (aux (cdr funs))))
                (if (parse-error? p2) p2 (cons p1 p2)))))
        (list)))
  (aux funs))

;;; Remarque : il est préférable de déterminiser la règle
(define (parse-instr parser)
  (let ((tok (parser 'peek)))
    (cond
      ((equal? tok 'return)
       (begin
         (parser 'next!)
         (let ((expr (parse-expr parser)))
           (if (parse-error? expr)
               expr
               (list 'return expr)))))
      ((equal? tok 'if)
       (begin
         (parser 'next!)
         (let ((p (parse-map parser
                             parse-expr
                             (lambda (parser) (parse-literal parser 'then))
                             parse-block
                             (lambda (parser) (parse-literal parser 'else))
                             parse-block)))
           (if (parse-error? p) p (list 'if (car p) (caddr p) (cadddr (cdr p)))))))
      (else (parse-expr parser)))))

(define (parse-params parser)
  (define (aux)
    (let ((e (parse-expr parser)))
      (if (parse-error? e) e
          (let ((tok (parser 'next!)))
            (cond
              ((equal? tok 'colon)
               (let ((rest (aux)))
                 (if (parse-error? rest) rest (cons e rest))))
              ((equal? tok 'rparen) (list e))
              (else (parse-error (str "expecting right-paren or colon, got '" tok "'"))))))))
  (parse-literal parser 'lparen)
  (let ((tok (parser 'peek)))
    (if (equal? tok 'rparen) (begin (parser 'next!) (list))
        (aux))))

(define (parse-expr parser)
  (let ((num (parse-num parser)))
    (if (parse-error? num)
        (let ((ident (parse-ident parser)))
          (if (parse-error? ident)
              (parse-error "expecting an expression")
              (let ((tok (parser 'peek)))
                (if (equal? tok 'lparen)
                    (let ((params (parse-params parser)))
                      (if (parse-error? params)
                          params
                          (cons ident params)))
                    ;; not a left parenthesis => variable occurrence
                    ident))))
        ;; a number
        num)))


(define test-tokens1
  '(function
    (sym identity)
    lparen
    (sym x)
    rparen
    lcurly
    return
    (sym x)
    semi
    rcurly
    (sym identity)
    lparen
    (num 42)
    rparen
    semi))

;;(parse-prog (mk-parser test-tokens1))


(define (new-parse-expr parser)
  (parse-addexpr parser))

(define (parse-addexpr parser)
  (let ((mul (parse-mulexpr parser)))
    (if (parse-error? mul)
        mul
        (let ((add (parse-addexpr2 parser)))
          (cond ((parse-error? add) add)
                ((not add) mul)
                (else (list (car add) mul (cadr add))))))))

(define (parse-addexpr2 parser)
  (let ((tok (parser 'peek)))
    (if (or (eq? tok '+)
            (eq? tok '-))
        (begin (parser 'next!)
               (let ((mul (parse-mulexpr parser)))
                 (if (parse-error? mul)
                     mul
                     (let ((add (parse-addexpr2 parser)))
                       (cond ((parse-error? add) add)
                             ((not add) (list tok mul))
                             (else (list tok (list (car add) mul (cadr add)))))))))
        ;; <empty> case
        #f)))

(define (parse-mulexpr parser)
  (let ((term (parse-termexpr parser)))
    (if (parse-error? term)
        term
        (let ((mul (parse-mulexpr2 parser)))
          (cond ((parse-error? mul) mul)
                ((not mul) term)
                (else (list (car mul) term (cadr mul))))))))


(define (parse-mulexpr2 parser)
  (let ((tok (parser 'peek)))
    (if (or (eq? tok '*)
            (eq? tok '/))
        (begin (parser 'next!)
               (let ((term (parse-expexpr parser)))
                 (if (parse-error? term)
                     term
                     (let ((mul (parse-mulexpr2 parser)))
                       (cond ((parse-error? mul) mul)
                             ((not mul) (list tok term))
                             (else (list tok (list (car mul) term (cadr mul)))))))))
        ;; <empty> case
        #f)))

(define (parse-expexpr parser)
  (let ((term (parse-termexpr parser)))
    (if (parse-error? term)
        term
        (let ((exp (parse-expexpr2 parser)))
          (cond ((parse-error? exp) exp)
                ((not exp) term)
                (else (list (car exp) term (cadr exp))))))))


(define (parse-expexpr2 parser)
  (let ((tok (parser 'peek)))
    (if (eq? tok '^)
        (begin (parser 'next!)
               (let ((term (parse-termexpr parser)))
                 (if (parse-error? term)
                     term
                     (let ((exp (parse-expexpr2 parser)))
                       (cond ((parse-error? exp) exp)
                             ((not exp) (list tok term))
                             (else (list tok (list (car exp) term (cadr exp)))))))))
        ;; <empty> case
        #f)))

(define (parse-termexpr parser)
  (let ((num (parse-num parser)))
    (if (parse-error? num)
        (let ((lparen (parse-literal parser 'lparen)))
          (if (parse-error? lparen)
              (let ((minus (parse-literal parser '-)))
                (if (parse-error? minus)
                    (let ((ident (parse-ident parser)))
                      (if (parse-error? ident)
                          (parse-error "expecting an expression")
                          (let ((tok (parser 'peek)))
                            (if (equal? tok 'lparen)
                                (let ((expr (begin (parser 'next!)
                                                   (parse-expr parser))))
                                  (if (parse-error? expr)
                                      expr
                                      (let ((res (parse-literal parser 'rparen)))
                                        (if (parse-error? res)
                                            res
                                            (list ident expr)))))
                                ;; not a left parenthesis => variable occurrence
                                ident))))
                    ;; a minus sign
                    (let ((expr (parse-expr parser)))
                      (if (parse-error? expr)
                          expr
                          (list '- expr)))))
              ;; left parenthesis
              (let ((expr (parse-expr parser)))
                (if (parse-error? expr)
                    expr
                    (let ((rparen (parse-literal parser 'rparen)))
                      (if (parse-error? rparen)
                          rparen
                          expr))))))
        ;; num
        num)))

(new-parse-expr (mk-parser '((num 42))))
(new-parse-expr (mk-parser '((num 42) + (num 3))))
(new-parse-expr (mk-parser '((num 42) + (num 3) * (num 9))))
(new-parse-expr (mk-parser '((num 42) * (num 3) ^ (num 9))))


(define (parsing filename input)
  (let ((tokens (lexing filename input)))
    (if (lex-error? tokens)
        (parse-error (cadr tokens))
        (let* ((parser (mk-parser tokens))
               (res (parse-prog parser)))
          (if (parser 'eof?)
              res
              (parse-error (str "Unexpected trailing tokens: " (parser 'fetch))))))))


(define identity-ex
  "function identity(x) {
  return x;
}

identity(42);")

(parsing "<string>" identity-ex)


(define identity-ex2
  "function identity(x) {
  return x;
}

identity(42 * 4 + 9);")

;; (parsing "<string>" identity-ex2)

(define funcall-ex1 "f(1);")
(parsing "<string>" funcall-ex1)

(define funcall-ex2 "f(1, 2, 3, 4);")
(parsing "<string>" funcall-ex2)

(define funcall-ex3 "f();")
(parsing "<string>" funcall-ex3)

(define ifthenelse-ex "if true then { 41; } else { 42; };")
(parsing "<string>" ifthenelse-ex)