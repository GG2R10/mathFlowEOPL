#lang eopl
(require "mathflow-grammar.scm")
(require "mathflow-types.scm")
(require "mathflow-env.scm")
(require "mathflow-eval.scm")

; Archivo main del interpretador

;; eval-program: program -> value
;; Evalua un programa usando el ambiente inicial y devuelve el valor resultante. 
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (let ((res (eval-expression body (init-env))))
                   (result-val res))))))

;; El loop REPL del interpretador
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;; Aquí lo ejecutamos para que pueda ser usado
(interpretador)
