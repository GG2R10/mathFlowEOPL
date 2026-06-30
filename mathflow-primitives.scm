#lang eopl
(require racket/vector)
(require "mathflow-grammar.scm")
(require "mathflow-types.scm")

; primitivas

; auxiliar para la funcion auxliar de abajo.
; devuelve false si todos los elementos son algo diferente a expresiones simbolicas. 
(define symbolic-args?
  (lambda (args)
    (let loop ((xs args))
      (if (null? xs)
          #f
          (or (symbolic-expval? (car xs))
              (loop (cdr xs)))))))

; usado para evitar, como se pone en las especificaciones del proyecto, que se hagan operaciones booleanas o comparativas con expresiones simbolicas.
(define ensure-non-symbolic-args
  (lambda (who args)
    (if (symbolic-args? args)
        (eopl:error who
                    "Operation does not support symbolic operands: ~s"
                    args)
        #t)))

; si es un expval simbolico usamos la funcion auxiliar que los convierte a string. Si es cualquier otro valor usamos display normal.
(define display-expval
  (lambda (v)
    (if (symbolic-expval? v)

        (display (expval->symbolic-string v))
        ; else
        (display v))))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ;; aritmeticas
      (add-prim ()
        (if (symbolic-args? args)
            (symbolic-exp '+ (car args) (cadr args))
            (+ (car args) (cadr args))))
      (subtract-prim ()
        (if (symbolic-args? args)
            (symbolic-exp '- (car args) (cadr args))
            (- (car args) (cadr args))))
      (mult-prim ()
        (if (symbolic-args? args)
            (symbolic-exp '* (car args) (cadr args))
            (* (car args) (cadr args))))
      (div-prim ()
        (if (symbolic-args? args)
            (symbolic-exp '/ (car args) (cadr args))
            (/ (car args) (cadr args))))
      
      (mod-prim ()
        (if (symbolic-args? args)
            (symbolic-exp '% (car args) (cadr args))
            (let ((lhs (car args))
                  (rhs (cadr args)))
              (if (and (integer-valued? lhs) (integer-valued? rhs))
                  (modulo lhs rhs)
                  (eopl:error 'mod-prim "mod expects integer operands, got ~s and ~s" lhs rhs)))))

      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      
      ;; operaciones sobre strings
      (strlen-prim () (string-length (car args)))
      (strcat-prim () (string-append (car args) (cadr args)))
      
      ;; booleanos
      (lt-prim () (begin (ensure-non-symbolic-args 'lt-prim args) (< (car args) (cadr args))))
      (gt-prim () (begin (ensure-non-symbolic-args 'gt-prim args) (> (car args) (cadr args))))
      (le-prim () (begin (ensure-non-symbolic-args 'le-prim args) (<= (car args) (cadr args))))
      (ge-prim () (begin (ensure-non-symbolic-args 'ge-prim args) (>= (car args) (cadr args))))
      (eq-prim () (begin (ensure-non-symbolic-args 'eq-prim args) (equal? (car args) (cadr args))))  
      (ne-prim () (begin (ensure-non-symbolic-args 'ne-prim args) (not (equal? (car args) (cadr args)))))
      (and-prim () (begin (ensure-non-symbolic-args 'and-prim args) (and (car args) (cadr args))))
      (or-prim () (begin (ensure-non-symbolic-args 'or-prim args) (or (car args) (cadr args))))
      (not-prim () (begin (ensure-non-symbolic-args 'not-prim args) (not (car args))))
      
      ;; listas usando el datatype `listval`
      (empty-list?-prim () (cases listval (car args)
                              (empty-list-val () #t)
                              (list-cons (h t) #f)))

      (make-list-prim () (list-cons (car args) (cadr args)))

      (list?-prim () (listval? (car args)))

      (head-prim () (cases listval (car args)
                      (empty-list-val () (eopl:error 'head-prim "Cannot take head of empty list"))
                      (list-cons (h t) h)))

      (tail-prim () (cases listval (car args)
                      (empty-list-val () (eopl:error 'tail-prim "Cannot take tail of empty list"))
                      (list-cons (h t) t)))

      (append-prim () (letrec ((loop (lambda (lst acc)
                                      (cases listval lst
                                        (empty-list-val () acc)
                                        (list-cons (h t) (list-cons h (loop t acc)))))))
                        (loop (car args) (cadr args))))

      (ref-list-prim () (let ((lst (car args)) (idx (cadr args)))
                          (if (integer-valued? idx)
                              (let loop ((l lst) (i 0))
                                (cases listval l
                                  (empty-list-val () (eopl:error 'ref-list-prim "Index out of bounds: ~s" idx))
                                  (list-cons (h t) (if (= i idx) h (loop t (+ i 1))))))
                              (eopl:error 'ref-list-prim "List index must be an integer, got ~s" idx))))

      (set-list-prim () (let ((lst (car args)) (idx (cadr args)) (val (caddr args)))
                          (if (integer-valued? idx)
                              (let loop ((l lst) (i 0))
                                (cases listval l
                                  (empty-list-val () (eopl:error 'set-list-prim "Index out of bounds: ~s" idx))
                                  (list-cons (h t) (if (= i idx) (list-cons val t) (list-cons h (loop t (+ i 1)))))))
                              (eopl:error 'set-list-prim "List index must be an integer, got ~s" idx))))
      
      ;; diccionarios
      (make-dict-prim ()
        (let loop ((remaining args) (acc '()))
          (if (null? remaining)
              (dict-val (reverse acc))
              (let ((key (car remaining)) (val (cadr remaining)))
                (unless (dict-key-value? key)
                  (eopl:error 'make-dict-prim "Dictionary keys must be strings or numbers, got: ~s" key))
                (loop (cddr remaining) (cons (cons key val) acc))))))

      (dict?-prim () (dictval? (car args)))

      (ref-dict-prim ()
        (let ((dict (car args)) (key (cadr args)))
          (cases dictval dict
            (dict-val (pairs)
              (let loop ((ps pairs))
                (cond ((null? ps) 'null)
                      ((equal? (caar ps) key) (cdar ps))
                      (else (loop (cdr ps)))))))))

      (set-dict-prim ()
        (let ((dict (car args)) (key (cadr args)) (val (caddr args)))
          (cases dictval dict
            (dict-val (pairs)
              (let loop ((ps pairs) (acc '()))
                (cond ((null? ps)
                       (dict-val (reverse (cons (cons key val) acc))))
                      ((equal? (caar ps) key)
                       (dict-val (append (reverse acc) (cons (cons key val) (cdr ps)))))
                      (else (loop (cdr ps) (cons (car ps) acc)))))))))

      (keys-prim ()
        (let ((dict (car args)))
          (cases dictval dict
            (dict-val (pairs)
              (letrec ((loop (lambda (ps acc)
                                (if (null? ps)
                                    acc
                                    (loop (cdr ps) (list-cons (caar ps) acc))))))
                (loop pairs (empty-list-val)))))))

      (values-prim ()
        (let ((dict (car args)))
          (cases dictval dict
            (dict-val (pairs)
              (letrec ((loop (lambda (ps acc)
                                (if (null? ps)
                                    acc
                                    (loop (cdr ps) (list-cons (cdar ps) acc))))))
                (loop pairs (empty-list-val)))))))
      
      ;; print lol
      (print-prim ()
                  (display-expval (car args))
                  (newline)
                  'null)
      
      ;; por si las moscas
      (else (eopl:error 'apply-primitive "Unknown primitive ~s" prim)))))

;; Export a los otros
(provide
    (all-defined-out)
    ; Aqui los excepts
)

