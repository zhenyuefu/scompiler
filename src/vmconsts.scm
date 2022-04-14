;;;;;;
;;;;;; SCompiler / Génération des constantes pour la SVM
;;;;;;
;;;;;; Sorbonne Université -- licence informatique 3ème année
;;;;;; 2021-
;;;;;; LU3IN018: Compilation
;;;;;;
;;;;;; Copyright (C) F.P. under GPLv3.0 (cf. LICENSE) 

;;; Attention, ce fichier utilise des extensions spécifiques à racket
#lang racket

(require "libbootstrap.scm")
(require "prims.scm")
(require "bytecode.scm")

(provide generate-vm-constants)

(define (generate-header file)
  (display "/* fichier genere automatiquement : *ne pas editer* */" file)
  (newline file)
  (display "\n/* Constantes pour les opcodes */\n" file)
  (for-each (lambda (opcode)
              (newline file)
              (display "/** opcode " file) (display (car opcode) file) (display " */\n" file)
              (display "#define I_" file) (display (car opcode) file) (display " " file) (display (cadr opcode) file) (newline file))
            op-codes)
  (display "\n/** Noms des opcodes */\n" file)
  (display "extern const char *opcode_names[];\n" file)
  (newline file)
  (display "\n/* Constantes pour les types */\n" file)
  (for-each (lambda (typcode)
              (newline file)
              (display "/** type " file) (display (car typcode) file) (display " */\n" file)
              (display "#define T_" file) (display (car typcode) file) (display " " file) (display (cadr typcode) file) (newline file))
            type-codes)
  (display "\n/** Noms des types */\n" file)
  (display "extern const char *type_names[];\n" file)
  (display "\n/* Constantes pour les primitives */\n" file)
  (for-each (lambda (primcode)
              (newline file)
              (display "/** primitive " file) (display (car primcode) file) (display " */\n" file)
              (display "#define P_" file) (display (caddr primcode) file) (display " " file) (display (cadr primcode) file) (newline file))
            (init-prims))
  (display "\n/** Noms des primitive */\n" file)
  (display "extern const char *primitive_names[];\n" file)
  (newline file))


(define (generate-module file)
  (display "/* fichier genere automatiquement : *ne pas editer* */" file)
  (newline file)
  (display "\n/** Noms des opcodes */\n" file)
  (display "const char *opcode_names[] = {\n" file)
  (for-each (lambda (opcode)
              (display "  \"" file) (display (string-downcase (symbol->string (car opcode))) file) (display "\",\n" file))
            (cdr op-codes)) ;; forget about LABEL
  (display "  \"<unknown>\"\n" file)
  (display "};\n" file)
  (display "\n/** Noms des types */\n" file)
  (display "const char *type_names[] = {\n" file)
  (for-each (lambda (typcode)
              (display "  \"" file) (display (string-downcase (symbol->string (car typcode))) file) (display "\",\n" file))
            type-codes)
  (display "  \"<unknown>\"\n" file)
  (display "};\n" file)
  (display "\n/** Noms des primitives */\n" file)
  (display "const char *primitive_names[] = {\n" file)
  (for-each (lambda (primcode)
              (display "  \"" file) (display (string-downcase (symbol->string (car primcode))) file) (display "\",\n" file))
            (init-prims))
  (display "  \"<unknown>\"\n" file)
  (display "};\n" file))


(define (generate-vm-constants)
  (let ((hfile (open-output-file "constants.h"))
        (cfile (open-output-file "constants.c")))
    (println "==> Generating header file: constants.h")
    (generate-header hfile)
    (close-output-port hfile)
    (println "==> Generating module file: constants.c")
    (generate-module cfile)
    (close-output-port cfile)
    (println "Done")))
