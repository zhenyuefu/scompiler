;;;;;;
;;;;;; SCompiler / Programme principal du compilateur
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0 (cf. LICENSE) 

;;; Attention, ce fichier utilise des extensions spécifiques à racket
#lang racket

(require "libbootstrap.scm")
(require "utils.scm")
(require "syntax.scm")
(require "expander.scm")
(require "compilenoyau.scm")
(require "bytecode.scm")
(require "serializer.scm")
(require "vmconsts.scm")
(require "reader.scm")

(define *generate-asm* #f)
(define *expand-only* #f)
(define *no-expand* #f)
(define *gen-vm-consts* #f)
(define *dest-file-base* "compiled")
(define *source-file* #f)

(define *loaded-files* (list))

(define (remove-provides prog)
  (filter (lambda (expr) (not (provide-expr? expr))) prog))

(define (main argv)
  (print-header)
  (cmd-args argv)

  (when *gen-vm-consts*
    (println "Generate constant files for virtual machine")
    (generate-vm-constants)
    (exit 0))

  (when (not *source-file*)
    (println "Error : source file not provided")
    (help)
    (exit 1))
  
  (let ((prog (begin
                (println (string-append "Loading " *source-file*))
                (remove-provides (load-program *source-file*)))))
    (let ((prog (if *no-expand*
                    (begin (println "Expander deactivated")
                           prog)
                    (begin (println "Expanding program ...")
                           (let ((eprog (map expander prog)))
                             (println "... expanding done")
                             (if *expand-only*
                                 (let ((dest-fname (string-append *dest-file-base* "-exp.scm")))
                                   (println (string-append "Generating expanded source: " dest-fname))
                                   (save-exp-file dest-fname eprog)
                                   (exit 0))
                                 ;; not just expansion
				 eprog))))))

      (let ((asmcode (begin
                       (println "Compiling program ...")
                       (compile-prog prog))))
        (println "... compilation done")


        (if *generate-asm*
            (begin
              (let ((dest-fname (string-append *dest-file-base* ".asm"))) 
                (println (string-append "Generating assembly file: " dest-fname))
                (save-asm-file dest-fname asmcode)))
            ;; else generate bytecode
            (let ((dest-fname (string-append *dest-file-base* ".bc"))) 
              (println (string-append "Generating bytecode file: " dest-fname))
              (let ((bytecode (begin
                                (println "Serializing assembly to bytecode ...")
                                (serialize-bytecode asmcode))))
                (println (string-append " ... Serialization done, generating bytecode file: " dest-fname))
                (save-bc-file dest-fname bytecode))))
        (println "Compilation finished")
        (exit 0)))))

(define (help)
  (println "----")
  (println "   scompiler [options] <file>.scm\n")
  (println "Command line options:")
  (println "  -h  --help      : this help message")
  (println "  -a  --asm       : generate assembly code")
  (println "  --expand-only   : generate expanded source only")
  (println "  --no-expand     : does not expand source")
  (println "  --gen-vm-consts : generate constants for VM")
  (println "----"))

(define (handle-option opt)
  (cond
   ;; aide
   ((or (equal? opt "--help")
        (equal? opt "-h"))
    (println "Help:")
    (help)
    (exit 0))
   
   ;; assembly
   ((or (equal? opt "--asm")
	(equal? opt "-a"))
    (set! *generate-asm* #t))
   
   ;; expand-only
   ((equal? opt "--expand-only")
    (set! *expand-only* #t))  
   
   ;; no-expand
   ((equal? opt "--no-expand")
    (set! *no-expand* #t)) 
   
   ;; gen-vm-consts
   ((equal? opt "--gen-vm-consts")
    (set! *gen-vm-consts* #t))
   
   ((not (string-prefix? opt "-"))
    (when (not (string-suffix? opt ".scm"))
      (println "Error: the source file must have extension: .scm")
      (exit 1))
    
    (set! *source-file* opt)
    (set! *dest-file-base* 
	  (substring opt 0 (- (string-length opt) 4))))
   
   (else (println (string-append "Does not understand option: " opt))
	 (exit 0))))

(define (cmd-args args)
  (when (pair? args)
    (handle-option (car args))
    (cmd-args (cdr args))))

(define (print-header)
  (println "================================")
  (println " Compilateur Scheme (scompiler) ")
  (println "================================"))

(define (load-program fname)
  (define (load-file dirname fname acc)
    (define (read-loop file acc)
      (let ((expr (read file)))
        (if (eof-object? expr) acc
            (if (require-expr? expr)
                (read-loop file (load-file dirname (require-path expr) acc))
                (read-loop file (cons expr acc))))))

    (if (not (member fname *loaded-files*))
        (begin
          (set! *loaded-files* (cons fname *loaded-files*))
          (let ((file (open-input-file (reroot-path dirname fname))))
            (let ((acc (read-loop file acc)))
              (close-input-port file)
              acc)))
          acc))

  (reverse (load-file (dir-path fname) (base-path fname) (list))))

(define (save-asm-file fname asmcode)
  ;; https://stackoverflow.com/questions/36869976/racket-how-do-i-remove-parentheses-and-apostrophe-from-lists
  (define (display-clean-list lst file)
    (when (pair? lst)
      (display (car lst) file)
      (when (pair? (cdr lst)) (display " " file))
      (display-clean-list (cdr lst) file)))

  (define (display-instr instr file)
    (cond
      ((BC-LABEL? instr)
       (display (BC-LABEL-label instr) file)
       (display ":" file))
      (else
       (begin (display " " file)
              (display-clean-list (flatten instr) file))))
    (display "\n" file))

  (let ((file (open-output-file fname)))
    (display ";;; assembly compiled from file: " file)
    (display (string-concat *loaded-files* ", ") file)
    (display "\n" file)
    (for-each (lambda (instr) (display-instr instr file)) asmcode)
    (close-output-port file)))

(define (save-bc-file fname bytecode)
  (let ((file (open-output-file fname))
        (symtable (car bytecode))
        (bytecode (cadr bytecode)))
    (display 424242 file) ;; magic
    (display " " file)
    (display (length bytecode) file) ;; longueur du bytecode
    (display " " file)
    (output-bc file bytecode)
    (close-output-port file)))

(define (output-bc port bytecode)
  (when (pair? bytecode)
    (display (car bytecode) port)
    (display " " port)
    (output-bc port (cdr bytecode))))

(define (save-exp-file fname prog)
  (let ((file (open-output-file fname)))
    (display ";;; expanded program from file: " file)
    (display (string-concat *loaded-files* ", ") file)
    (display "\n" file)
    (for-each (lambda (expr)
                (begin (display expr file)
                       (display "\n" file))) prog)
    (close-output-port file)))

;; lancement du compilateur
(main (vector->list (current-command-line-arguments)))
