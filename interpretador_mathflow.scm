#lang eopl
(require racket/vector)

; MathFlow interpreter base
; Tomé como base el interprete anterior y le agregue const, var y asignacion.
; La propagacion del ambiente en secuencias queda para mas adelante.

; scanner heredado
(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (string
     ("\"" (arbno (not #\")) "\"") string)
    (identifier
     (letter (arbno (or letter digit "?"))) symbol)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number)))

; grammar con var, const y asignacion
(define grammar-simple-interpreter
  '((program (expression) a-program)
    (expression (number) lit-exp)
    (expression (string) str-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    (expression ("vacio") empty-list-exp)
    (expression (identifier id-suffix) id-dispatch)
    (expression
     (primitive "(" (separated-list expression ",")")")
     primapp-exp)
    (expression ("if" expression "then" expression "else" expression "end")
                if-exp)
    (expression ("func" identifier "(" (separated-list identifier ",") ")" "{" (arbno expression ";") "return" expression "}") 
                func-exp)
    (expression ("(" expression ")") group-exp)
    ; extras del lenguaje
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-exp)
    ; asignacion como sufijo
    (id-suffix ("=" expression) assign-suffix)
    (id-suffix ("(" (separated-list expression ",") ")") call-suffix)
    (id-suffix () empty-suffix)

    ; nuevas declaraciones
    (expression ("var" identifier "=" expression) var-decl-exp)
    (expression ("const" identifier "=" expression) const-decl-exp)

    ; bucles y control
    (expression ("while" expression "do" expression "done") while-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)

    ; estructuras de datos
    (expression ("[" (separated-list expression ",") "]") list-exp)
    (expression ("{" (separated-list dict-pair ",") "}") dict-exp)
    (dict-pair (identifier ":" expression) pair-exp)

    ; simbolos y primitivas propias
    (expression ("symbol" identifier) symbol-exp)
    (expression ("simplificar" "(" expression ")") simplify-exp)
    (expression ("evaluar" "(" expression "," (separated-list binding ",") ")") evaluate-exp)
    (binding (identifier "=" expression) binding-exp)

    (primitive ("+") add-prim)
    (primitive ("-") substract-prim)
    (primitive ("*") mult-prim)
    (primitive ("/") div-prim)
    (primitive ("%") mod-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    (primitive ("longitud") strlen-prim)
    (primitive ("concatenar") strcat-prim)
    (primitive ("<") lt-prim)
    (primitive (">") gt-prim)
    (primitive ("<=") le-prim)
    (primitive (">=") ge-prim)
    (primitive ("==") eq2-prim)
    (primitive ("<>") ne-prim)
    (primitive ("and") and-prim)
    (primitive ("or") or-prim)
    (primitive ("not") not-prim)
    (primitive ("vacio?") empty-list?-prim)
    (primitive ("crear-lista") make-list-prim)
    (primitive ("lista?") list?-prim)
    (primitive ("cabeza") head-prim)
    (primitive ("cola") tail-prim)
    (primitive ("append") append-prim)
    (primitive ("ref-list") ref-list-prim)
    (primitive ("set-list") set-list-prim)
    (primitive ("crear-diccionario") make-dict-prim)
    (primitive ("diccionario?") dict?-prim)
    (primitive ("ref-diccionario") ref-dict-prim)
    (primitive ("set-diccionario") set-dict-prim)
    (primitive ("claves") keys-prim)
    (primitive ("valores") values-prim)
    (primitive ("print") print-prim)))

; datatypes generados desde scanner y grammar
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

; front end
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

; targets y referencias
(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?))
  (const-target (expval expval?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

; ambiente base
(define (expval? x)
  (or (number? x)
      (boolean? x)
      (string? x)
      (symbol? x) ; null se guarda como 'null
      (procval? x)
      (list? x)
      (pair? x)))


; tipos de ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

(define empty-env  (lambda () (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

(define rib-find-position (lambda (sym los) (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)
                    (const-target (v) #t)))))))

 (define deref
   (lambda (ref)
     (cases target (primitive-deref ref)
       (direct-target (expval) expval)
       (indirect-target (ref1)
                        (cases target (primitive-deref ref1)
                          (direct-target (expval) expval)
                          (indirect-target (p)
                                           (eopl:error 'deref
                                                       "Illegal reference: ~s" ref1))
                          (const-target (expval) expval)))
       (const-target (expval) expval))))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

; resultado como par valor-ambiente
(define make-result (lambda (v e) (cons v e)))
(define result-val car)
(define result-env cdr)

; setref protege const
(define setref!
  (lambda (ref expval)
    (primitive-setref! ref (direct-target expval))))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (let ((current (vector-ref vec pos)))
               (cases target current
                 (const-target (v) (eopl:error 'primitive-setref! "Cannot modify const"))
                 (direct-target (r) (vector-set! vec pos val))
                 (indirect-target (r) (vector-set! vec pos val))))))))

; evaluacion parcial
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) (make-result datum env))
      (str-exp (s) (make-result s env))
      (true-exp () (make-result #t env))
      (false-exp () (make-result #f env))
      (null-exp () (make-result 'null env))
      (empty-list-exp () (make-result '() env))
      (id-dispatch (id suffix)
                   (cases id-suffix suffix
                     (assign-suffix (rhs)
                                    (let ((res (eval-expression rhs env)))
                                      (let ((v (result-val res)) (env2 (result-env res)))
                                        (setref! (apply-env-ref env2 id) v)
                                        (make-result 1 env2))))
                     (empty-suffix () (make-result (apply-env env id) env))
                     (else (eopl:error 'eval-expression "Unknown id-suffix ~s" suffix))))
      (primapp-exp (prim rands)
                   (let ((res (eval-primapp-exp-rands rands env)))
                     (let ((args (result-val res)) (env2 (result-env res)))
                       (make-result (apply-primitive prim args) env2))))
      (if-exp (test-exp true-exp false-exp)
              (let ((res (eval-expression test-exp env)))
                (let ((v (result-val res)) (env2 (result-env res)))
                  (if (true-value? v)
                      (eval-expression true-exp env2)
                      (eval-expression false-exp env2)))))
      (group-exp (exp)
                 (eval-expression exp env))
      ; var: extiende el ambiente y devuelve el valor
      (var-decl-exp (id rhs)
                    (let ((res (eval-expression rhs env)))
                      (let ((v (result-val res)) (env2 (result-env res)))
                        (make-result v (extend-env (list id) (list (direct-target v)) env2)))))
      ; const: igual que var pero queda como const-target
      (const-decl-exp (id rhs)
                      (let ((res (eval-expression rhs env)))
                        (let ((v (result-val res)) (env2 (result-env res)))
                          (make-result v (extend-env (list id) (list (const-target v)) env2)))))
      (begin-exp (exp exps)
                 (let loop ((res (eval-expression exp env)) (exps exps))
                   (let ((env1 (result-env res)))
                     (if (null? exps)
                         res
                         (loop (eval-expression (car exps) env1) (cdr exps)))))))))

(define eval-rands
  (lambda (rands env)
    (let loop ((rs rands) (acc '()) (env env))
      (if (null? rs)
          (make-result (reverse acc) env)
          (let ((res (eval-rand (car rs) env)))
            (loop (cdr rs) (cons (result-val res) acc) (result-env res)))))))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (id-dispatch (id suffix)
                   (cases id-suffix suffix
                     (empty-suffix ()
                                   (let ((ref (apply-env-ref env id)))
                                     (make-result
                                      (cases target (primitive-deref ref)
                                        (direct-target (expval) ref)
                                        (indirect-target (ref1) ref1)
                                        (const-target (expval) ref))
                                      env)))
                     (else (eopl:error 'eval-rand "Invalid identifier form in argument: ~s" rand))))
      (else
       (let ((res (eval-expression rand env)))
         (make-result (direct-target (result-val res)) (result-env res)))))))

(define eval-primapp-exp-rands
  (lambda (rands env)
    (let loop ((rs rands) (acc '()) (env env))
      (if (null? rs)
          (make-result (reverse acc) env)
          (let ((res (eval-expression (car rs) env)))
            (loop (cdr rs) (cons (result-val res) acc) (result-env res)))))))

; primitivas
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ; aritmeticas
      (add-prim () (+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (mod-prim () (modulo (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      ; strings
      (strlen-prim () (string-length (car args))) 
      (strcat-prim () (string-append (car args) (cadr args)))
      ; booleanas y relacionales
      (lt-prim () (< (car args) (cadr args)))
      (gt-prim () (> (car args) (cadr args)))
      (le-prim () (<= (car args) (cadr args)))
      (ge-prim () (>= (car args) (cadr args)))
      (eq2-prim () (equal? (car args) (cadr args)))
      (ne-prim () (not (equal? (car args) (cadr args))))
      (and-prim () (and (car args) (cadr args)))
      (or-prim () (or (car args) (cadr args)))
      (not-prim () (not (car args)))
      ; listas como vectores
        (empty-list?-prim () (zero? (vector-length (car args))))
        (make-list-prim () (list->vector args)) ; pasa los args a vector
        (list?-prim () (vector? (car args)))
        (head-prim () (vector-ref (car args) 0))
        (tail-prim () (vector-drop (car args) 1)) ; usa racket/vector
        (append-prim () (vector-append (car args) (cadr args)))
        (ref-list-prim () (vector-ref (car args) (cadr args)))
        (set-list-prim ()
        (let ((vec (car args)) (idx (cadr args)) (val (caddr args)))
            (if (< idx (vector-length vec))
                (begin
                (vector-set! vec idx val)
                vec) ; devuelve el vector modificado
                (eopl:error 'set-list "Index out of bounds"))))

      ; diccionarios
        (make-dict-prim ()
        (let loop ((remaining args) (acc '()))
          (if (null? remaining)
            (list->vector (reverse acc)) ; convierte pares a vector
            (let ((key (car remaining)) (val (cadr remaining)))
            (unless (symbol? key)
              (eopl:error 'make-dict-prim "Dictionary keys must be identifiers or symbols"))
            (loop (cddr remaining) (cons (cons key val) acc))))))
      (dict?-prim () (vector? (car args)))
        (ref-dict-prim ()
        (let ((dict (car args)) (key (cadr args)))
          (let loop ((i 0))
          (if (>= i (vector-length dict))
            (eopl:error 'get-dict "Key not found in dictionary")
            (let ((pair (vector-ref dict i)))
              (if (eqv? (car pair) key)
                (cdr pair) ; devuelve el valor asociado
                (loop (+ i 1))))))))
        (set-dict-prim ()
        (let ((dict (car args)) (key (cadr args)) (val (caddr args)))
          (let loop ((i 0))
          (if (>= i (vector-length dict))
            (eopl:error 'set-dict "Key not found to update")
            (let ((pair (vector-ref dict i)))
              (if (eqv? (car pair) key)
                (begin
                ; reemplaza el par viejo por uno nuevo
                (vector-set! dict i (cons key val)) 
                dict) ; devuelve el diccionario modificado
                (loop (+ i 1))))))))
      (keys-prim ()
        (map car (vector->list (car args))))
      (values-prim ()
                  (map cdr (vector->list (car args))))
      ; salida
      (print-prim ()
                  (display (car args))
                  (newline)
                  'null)
      (else (eopl:error 'apply-primitive "Unknown primitive ~s" prim)))))
; procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

(define apply-procedure
  (lambda (proc args caller-env)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

; reglas de verdad
(define true-value?
  (lambda (x)
    (not (or (eq? x 'null) (equal? x 0) (equal? x "") (equal? x #f)))))

; programa y repl
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (let ((res (eval-expression body (init-env))))
                   (result-val res))))))

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      scanner-spec-simple-interpreter
      grammar-simple-interpreter)))

; no lo lanzo aca porque este archivo es de trabajo
; (interpretador)