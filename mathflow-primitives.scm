#lang eopl
(require racket/vector)
(require "mathflow-grammar.scm")
(require "mathflow-types.scm")

; primitivas

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ;; aritmeticas
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))  
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (mod-prim () (modulo (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      
      ;; operaciones sobre strings
      (strlen-prim () (string-length (car args)))
      (strcat-prim () (string-append (car args) (cadr args)))
      
      ;; booleanos
      (lt-prim () (< (car args) (cadr args)))
      (gt-prim () (> (car args) (cadr args)))
      (le-prim () (<= (car args) (cadr args)))
      (ge-prim () (>= (car args) (cadr args)))
      (eq-prim () (equal? (car args) (cadr args)))  
      (ne-prim () (not (equal? (car args) (cadr args))))
      (and-prim () (and (car args) (cadr args)))
      (or-prim () (or (car args) (cadr args)))
      (not-prim () (not (car args)))
      
      ;; listas (vamos a hacerlas con vectores para mutabilidad)
      
      ;; diccionarios (vamos a hacerlos con vectores para mutabilidad)
      
      ;; print lol
      (print-prim ()
                  (display (car args))
                  (newline)
                  'null)
      
      ;; por si las moscas
      (else (eopl:error 'apply-primitive "Unknown primitive ~s" prim)))))

;; Export a los otros
(provide
    (all-defined-out)
    ; Aqui los excepts
)

