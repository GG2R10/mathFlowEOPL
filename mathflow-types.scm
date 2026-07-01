#lang eopl
(require "mathflow-grammar.scm")

; Tipos y estructuras de datos
; son las definiciones 

;; predicados de los tipos de datos
(define (expval? x)
  (or (number? x)
      (boolean? x)
      (string? x)
      (symbol? x)         ; null representado como 'null
      (procval? x)
      (symval? x)         ; simbolos algebraicos
      (symexpr? x)        ; expresiones algebraicas
      (listval? x)        ; datatype de listas
      (dictval? x)))      ; datatype para diccionarios

;; claves permitidas en diccionarios ya evaluados
(define (dict-key-value? x)
  (or (string? x)
      (number? x)))

(define (switch-case-value? x)
  (or (number? x)
      (string? x)
      (boolean? x)
      (symbol? x)))

;; enteros del lenguaje: se usa para validar operaciones que deben recibir enteros
(define (integer-valued? x)
  (and (number? x)
       (integer? x)))

;; target: reconocedor de tipo almacenado para las referencias mutables
;; usado por el environment para almacenar valores o referencias a valores (solo hay un nivel de profundidad de referencias).
;; El const-target protege contra la mutacion (las operaciones de asignacion fallan :s)
(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?))
  (const-target (expval expval?)))

;; referencia: una estructura con la posicion de uno de los vectores de ambiente y el vector mismo.
(define-datatype reference reference?
  (a-ref 
    (position integer?)
    (vec vector?)
  )
)

;; funcion auxiliar para verificar si una referencia apunta a un target directo
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)
                    (const-target (v) #t)))))))

;; Ambiente: tabla de simbolos con vector mutable de valores
;; Son 2 listas paralelas: (x y z) -> (v1 v2 v3) y un apuntador al ambiente que lo contiene
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

; procval: el closure captura los ids, el cuerpo y el entorno de definicion 
;; Las funciones son recursivas por defecto: su nombre esta ligado en los entornos de sus propios cierres o referencias a valores. 
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;; symval: variable matematica declarada con `symbol x`
(define-datatype symval symval?
  (math-symbol (id symbol?)))

;; symexpr: expresion algebraica simbolica construida por primitivas aritmeticas
(define-datatype symexpr symexpr?
  (symbolic-exp
   (op symbol?)
   (lhs expval?)
   (rhs expval?)))

;; listval: representacion de listas del lenguaje (no usar pair? ni list? de racket)
(define-datatype listval listval?
  (empty-list-val)
  (list-cons (head expval?) (tail listval?)))

;; dictval: representacion de diccionarios como lista de parejas (key . value)
(define-datatype dictval dictval?
  (dict-val (pairs (list-of pair?))))

;; mini funciones auxiliares para hacer los pares de (resultado ambiente) que se usan en la evaluacion de expresiones y declaraciones
(define make-result (lambda (v e) (cons v e)))
(define result-val car)
(define result-env cdr)

;; utilidades para algebra simbolica

; verifica si un valor es un simbolo o una expresion simbolica
(define (symbolic-expval? v)
  (or (symval? v)
      (symexpr? v)))

; convierte una expresion simbolica o un simbolo a un string para imprimirlo 
(define expval->symbolic-string
  (lambda (v)
    (cond
      ((symval? v)
       (cases symval v
         (math-symbol (id) (symbol->string id))))
      ((symexpr? v)
       (cases symexpr v
         (symbolic-exp (op lhs rhs)
           (string-append
            (symbol->string op)
            "("
            (expval->symbolic-string lhs)
            ","
            (expval->symbolic-string rhs)
            ")"))))
      ((number? v) (number->string v))
      ((symbol? v) (symbol->string v))
      ((boolean? v) (if v "true" "false"))
      ((string? v) (string-append "\"" v "\""))
      (else
       (eopl:error 'expval->symbolic-string
                   "Cannot stringify non-supported symbolic operand: ~s"
                   v)))))

;; por si acaso
(define scheme-value? (lambda (v) #t))

;; export a los otros
(provide
    (all-defined-out)
    ; Aqui los excepts
)
