(define (some-displays)
  (display 42)
  (display '(x 2 3))
  (display (list 1 2 3 4)))

(some-displays)

(current-command-line-arguments)

(define filename
  (if (pair? (current-command-line-arguments))
      (car (current-command-line-arguments))
      (error "needs a command line arg")))

(define (open-and-write filename)
  (let ((f (open-output-file filename)))
    (display '(x 2 3) f)
    (close-output-port f)))

(open-and-write filename)

(define (open-and-read-char filename)
  (let ((f (open-input-file filename)))
    (display (read-char f))
    (display (peek-char f))
    (display (peek-char f 2))
    (close-input-port f)))

(open-and-read-char filename)
