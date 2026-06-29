#lang eopl
(require "mathflow-grammar.scm")
(require "mathflow-types.scm")
(require "mathflow-env.scm")
(require "mathflow-eval.scm")

;; eval-program: program -> value
;; Evalua un programa usando el ambiente inicial y devuelve el valor resultante. 
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp)
                 (let ((res (eval-expression exp (init-env))))
                   (result-val res))))))

;; El loop REPL del interpretador
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

;; permite pasarle un texto en string que escanea y luego envia a eval-program
(define (run src)
  (eval-program (scan&parse src)))

;; export para repl y tests
(provide
    (all-defined-out)
    ; Aqui los excepts
)