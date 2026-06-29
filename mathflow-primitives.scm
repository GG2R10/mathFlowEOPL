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
      
      ;; listas (hechas con el estilo racket de cons)
      
      ;; la lista vacia es el simbolo 'empty_list. Esto es para que tengamos distincion del tipo lista.
      (empty-list?-prim () (eq? (car args) 'empty_list))

      ;; crear-lista(elem, lst) = cons
      (make-list-prim () (cons (car args) (cadr args)))

      ;; Una lista válida es un par o 'empty_list
      (list?-prim () (let loop ((x (car args)))
                      (cond ((eq? x 'empty_list) #t)
                            ((pair? x) (loop (cdr x)))
                            (else #f))))

      ;; cabeza = car
      (head-prim () (if (pair? (car args))
                        (car (car args))
                        (eopl:error 'head-prim "Cannot take head of empty list")))

      ;; cola = cdr
      (tail-prim () (if (pair? (car args))
                        (cdr (car args))
                        (eopl:error 'tail-prim "Cannot take tail of empty list")))

      ;; append recorre la primera lista reconstruyéndola sobre la segunda
      (append-prim () (let loop ((lst (car args)))
                        (if (eq? lst 'empty_list)
                            (cadr args)
                            (cons (car lst) (loop (cdr lst))))))

      ;; ref-list recorre la lista hasta el índice
      (ref-list-prim () (let ((lst (car args)) (idx (cadr args)))
                          (let loop ((l lst) (i 0))
                            (cond ((eq? l 'empty_list) (eopl:error 'ref-list-prim "Index out of bounds: ~s" idx))
                                  ((= i idx) (car l))
                                  (else (loop (cdr l) (+ i 1)))))))

      ;; set-list reconstruye la lista reemplazando el elemento en idx
      (set-list-prim () (let ((lst (car args)) (idx (cadr args)) (val (caddr args)))
                          (let loop ((l lst) (i 0))
                            (cond ((eq? l 'empty_list) (eopl:error 'set-list-prim "Index out of bounds: ~s" idx))
                                  ((= i idx) (cons val (cdr l)))
                                  (else (cons (car l) (loop (cdr l) (+ i 1))))))))
      
      ;; diccionarios
      (make-dict-prim ()
        (let loop ((remaining args) (acc '()))
          (if (null? remaining)
              (cons 'dict (reverse acc))
              (let ((key (car remaining)) (val (cadr remaining)))
                (unless (string? key)
                  (eopl:error 'make-dict-prim "Dictionary keys must be strings, got: ~s" key))
                (loop (cddr remaining) (cons (cons key val) acc))))))

      (dict?-prim ()
        (let ((v (car args)))
          (and (pair? v) (eq? (car v) 'dict))))

      (ref-dict-prim ()
        (let ((dict (cdr (car args))) (key (cadr args)))
          (let loop ((pairs dict))
            (cond ((null? pairs) 'null)
                  ((equal? (caar pairs) key) (cdar pairs))
                  (else (loop (cdr pairs)))))))

      (set-dict-prim ()
        (let ((dict (car args)) (key (cadr args)) (val (caddr args)))
          (let loop ((pairs (cdr dict)) (acc '()))
            (cond ((null? pairs)
                  (cons 'dict (reverse (cons (cons key val) acc))))
                  ((equal? (caar pairs) key)
                  (cons 'dict (append (reverse acc) (cons (cons key val) (cdr pairs)))))
                  (else (loop (cdr pairs) (cons (car pairs) acc)))))))

      (keys-prim ()
        (let loop ((pairs (cdr (car args))) (acc 'empty_list))
          (if (null? pairs)
              acc
              (loop (cdr pairs) (cons (caar pairs) acc)))))

      (values-prim ()
        (let loop ((pairs (cdr (car args))) (acc 'empty_list))
          (if (null? pairs)
              acc
              (loop (cdr pairs) (cons (cdar pairs) acc)))))
      
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

