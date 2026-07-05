#lang eopl

; Especificación lexica y sintactica (scanner y grammar)

;; scanner
;; Se encarga de definir los tokens y como se van a reconocer en el texto de entrada.
(define scanner-spec-simple-interpreter
  '((white-sp
     (whitespace) skip)
    (comment
     ("%" (arbno (not #\newline))) skip)
    (string
     ("\"" (arbno (not #\")) "\"") string) ; Esto genera que nuestros strings sean "\"texto\"", los tenemos que sanear eliminando las comillas del principio y final
    (identifier
     (letter (arbno (or letter digit "?"))) symbol)
    ;; números: enteros y flotantes, con opción de signo negativo
    (number
     (digit (arbno digit) "." digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (number
     (digit (arbno digit)) number)
    (number
     ("-" digit (arbno digit)) number))) ; OJOOO: el signo negativo se reconoce en el scanner, lo que puede generar ambiguedades con el operador de resta en algunos contextos.

;; Especificacion gramatica
;; Nos dice como se van a combinar los tokens para formar expresiones y programas validos.

;; notas :D:

;; - func-exp requiere ";" después de cada expresión intermedia para resolver conflictos LL(1).
;; - group-exp es un agrupamiento de una sola expresión; call-suffix maneja f(args).
;; - Las bifurcaciones de id-dispatch con id-suffix resuelven la ambiguedad de los identificadores (asignación vs. llamada vs. variable simple).
;; - binding/dict-pair utilizan no terminales separados en lugar de patrones en línea (limitación de SLLGEN con separated-list).

(define grammar-simple-interpreter
    '((program (expression) a-program)

    ;; literales
    (expression (number) lit-exp)
    (expression (string) str-exp)
    (expression ("true") true-exp)
    (expression ("false") false-exp)
    (expression ("null") null-exp)
    (expression ("vacio") empty-list-exp)
    
    ;; identificadores y sufijos
    (expression (identifier id-suffix) id-dispatch)
    (id-suffix ("=" expression) assign-suffix)
    (id-suffix ("(" (separated-list expression ",") ")") call-suffix)
    (id-suffix () empty-suffix)
    
    ;; llamadas a primitivas
    (expression (primitive "(" (separated-list expression ",") ")") primapp-exp)
    
    ;; estructuras de control
    (expression ("if" expression "then" expression "else" expression "end") if-exp)
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)  

    ;; nueva estructura de control: Switch 
    (expression ("switch" expression "{" (arbno case-clause) "default" "{" (separated-list expression ";") "}" "}") switch-exp)
    (case-clause ("case" expression "{" (separated-list expression ";") "}") case-clause-exp) 
    
    ;; declaracion de funciones. Segun el proshecto hay que tener la forma de llamarla con return y sin return
    (expression ("func" identifier "(" (separated-list identifier ",") ")" "{" (separated-list expression ";") func-return "}")
                func-exp)
    (func-return ("return" expression) func-return-exp)
    (func-return () empty-return-exp)
    
    ;; Call para funciones. Nos permite llamar a una funcion devuelta por una expresion
    (expression ("(" expression ")" "(" (separated-list expression ",") ")") call-exp)

    ;; declaraciones. En nuestro lenguaje expanden directamente el ambiente apenas son evaluadas
    (expression ("var*" identifier "=" identifier) var-ref-decl-exp) ;; creacion de puntero / alias
    (expression ("var" identifier "=" expression) var-decl-exp)
    (expression ("const" identifier "=" expression) const-decl-exp)

    ;; referencias en funciones
    (expression ("ref" identifier) ref-exp)
    
    ;; estructuras de ciclos
    (expression ("while" expression "do" expression "done") while-exp)
    (expression ("for" identifier "in" expression "do" expression "done") for-exp)
    
    ;; data structures paaa
    (expression ("[" (separated-list expression ",") "]") list-exp)
    (expression ("{" (separated-list dict-pair ",") "}") dict-exp)
    (dict-pair (expression ":" expression) pair-exp)
    
    ;; expresiones especiales de evaluacion y simplificacion algebraica
    (expression ("symbol" identifier) symbol-exp)
    (expression ("simplificar" "(" expression ")") simplify-exp)
    (expression ("evaluar" "(" expression "," (separated-list binding ",") ")") evaluate-exp)
    (binding (identifier "=" expression) binding-exp)
    
    ;; primitivas
    (primitive ("+") add-prim)
    (primitive ("-") subtract-prim) 
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
    (primitive ("==") eq-prim)     
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
    (primitive ("print") print-prim)
    )
)
;; generamos los datatypes en base a la gramatica
(sllgen:make-define-datatypes scanner-spec-simple-interpreter grammar-simple-interpreter)

;; front-end: parser y scan
(define scan&parse
  (sllgen:make-string-parser scanner-spec-simple-interpreter grammar-simple-interpreter))

(define just-scan
  (sllgen:make-string-scanner scanner-spec-simple-interpreter grammar-simple-interpreter))

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

;; Manejo de ambientes y referencias (memoria mutable)
;; Basicamente, el ambiente es una tabla (estructurada como 2 listas paralelas), una de simbolos y otra de referencias a valores. junto con un campo que apunta al ambiente anterior del que fue extendido
;; Las referencias son targets, los cuales dependiendo de su tipo y la definición del datatype target pueden ser directos o constantes (expvals) o indirectos (ref-to-direct-target).

;; nombre para el ambiente vacio :)
(define empty-env (lambda () (empty-env-record)))

;; funcion para extender ambientes con un vector de valores y una lista de simbolos. Devuelve el nuevo ambiente extendido que a su vez tiene la referencia al antiguo
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;; Extiende el ambiente recursivamente para procedimientos recursivos
;; Simplemente crea un nuevo ambiente extendido con los nombres de las funciones, y luego llena el vector de referencias con closures que apuntan al mismo ambiente extendido para que puedan llamarse a si mismos. Por ultimo, devuelve ese ambiente extendido.
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

;; funcion auxiliar para las iteraciones del for each sobre cada una de las funciones recursivas que se van a enlazar en el ambiente extendido
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;; Handling de referencias
;; primitive deref: extrae el target de una posición de un vector de ambiente
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

;; deref: sigue una cadena de referencias hasta llegar a un target directo y devuelve su valor
(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)

      ;; Nuestro sistema de optimizacion de ruta en aliases evita que lleguemos a esta linea si hacemos cosas como "var* y = x; var* z = y", 
      ;;ya que z no apunta al indirect-target y, sino que se lo salta y entonces apunta como indirect-target a x. Para ver mas, irse a la parte del evaluador para var-ref-decl-exp
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1))
                         (const-target (expval) expval)))

      (const-target (expval) expval))))

;; modificacion de referencias
;; setref!: wrapper para primitive-setref! que crea un target directo con el nuevo valor y lo asigna a la referencia
(define setref!
  (lambda (ref expval)
    (primitive-setref! ref (direct-target expval))))

;; Primitive setref: actualiza un target en el vector ambiente, siguiendo la cadena de referencias si es necesario. Si el target es const lanza un error :).
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (let ((current (vector-ref vec pos)))
               (cases target current
                 (const-target (v) (eopl:error 'primitive-setref! "Cannot modify const"))
                 (direct-target (r) (vector-set! vec pos val))
                 (indirect-target (r) (primitive-setref! r val))))))))

;; Aplicacion de ambientes 

;; apply-env: Obtiene el valor de un simbolo en el ambiente por medio de una desreferencia a la referencia que devuelve apply-env-ref
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

;; apply-env-ref: Obtiene la referencia a un simbolo en el ambiente (o sus padres) por medio de una busqueda recursiva en la lista de simbolos del ambiente. Si no lo encuentra lanza un error.
;; para entendernos: apply-env: Devuelve valor, apply-env-ref: Devuelve referencia
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))

      (extended-env-record (syms vals env)
                           (let ((pos (list-index (lambda (sym1) (eqv? sym1 sym)) syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))

;; predicados de bindings para validaciones semanticas (p.ej. symbol vs var/const)
(define target->expval
  (lambda (t)
    (cases target t
      (direct-target (v) v)
      (const-target (v) v)
      (indirect-target (r) (deref r)))))

(define env-has-binding?
  (lambda (env sym)
    (cases environment env
      (empty-env-record () #f)
      (extended-env-record (syms vals parent)
        (or (let loop ((ss syms) (idx 0))
              (cond
                ((null? ss) #f)
                ((eqv? (car ss) sym) #t)
                (else (loop (cdr ss) (+ idx 1)))))
            (env-has-binding? parent sym))))))

(define env-has-symbolic-binding?
  (lambda (env sym)
    (cases environment env
      (empty-env-record () #f)
      (extended-env-record (syms vals parent)
        (or (let loop ((ss syms) (idx 0))
              (cond
                ((null? ss) #f)
                ((eqv? (car ss) sym)
                 (or (symval? (target->expval (vector-ref vals idx)))
                     (loop (cdr ss) (+ idx 1))))
                (else (loop (cdr ss) (+ idx 1)))))
            (env-has-symbolic-binding? parent sym))))))

;; mejoramos las 3 funciones de rib-find-position a solo list-index, que es usada para buscar la posicion del simbolo en la lista de simbolos en apply-env-ref
(define list-index
  (lambda (pred ls)
    (let loop ((ls ls) (n 0))
      (cond
        ((null? ls) #f)
        ((pred (car ls)) n)
        (else (loop (cdr ls) (+ n 1)))))))

;; nombre para ambiente inicial. Antes tenia algunos direct-targets creados. Ahora está vacio
(define init-env
  (lambda ()
    (empty-env)))

; primitivas

; auxiliar para la funcion auxliar de abajo y para saber si en las primitivas tenemos una expresion normal o simbolica.
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

      ; intuitivo.
      (empty-list?-prim () (cases listval (car args)
                              (empty-list-val () #t)
                              (list-cons (h t) #f)))

      ;; intuitivo.
      (make-list-prim () (list-cons (car args) (cadr args)))

      ;; intuitivo.
      (list?-prim () (listval? (car args)))

      ;; si es vacia error. Si tenia cosas simplemente sacamos la cabeza. 
      (head-prim () (cases listval (car args)
                      (empty-list-val () (eopl:error 'head-prim "Cannot take head of empty list"))
                      (list-cons (h t) h)))

      ;; mismo de arriba, pero sacamos la cola ahora. 
      (tail-prim () (cases listval (car args)
                      (empty-list-val () (eopl:error 'tail-prim "Cannot take tail of empty list"))
                      (list-cons (h t) t)))

      ;; creamos un bucle con la primera lista y la segunda.
      ;; si la primera lista estaba vacia, devolvemos la segunda
      ;; si la primera lista tenia cosas, la vamos recreando
      ;; por ende, cuando se termine de rehacer la primera lista y se quede vacia, al final le agregaremos la segunda lista.
      ;; cabe aclarar que esto crea una nueva lista con la primera y segunda, no modifica ninguna de las anteriores. 
      (append-prim () 
        (letrec (
          (loop (lambda (lst acc)
            (cases listval lst
              (empty-list-val () acc)
              (list-cons (h t) (list-cons h (loop t acc)))))))

          (loop (car args) (cadr args))))

      ;; vamos iterando por un ciclo de contar y buscar en la tail hasta que lleguemos al indice que nos pasaron.
      ;; Si existia devolvemos el valor.  
      (ref-list-prim () 
        (let ((lst (car args)) (idx (cadr args)))
          (if (integer-valued? idx)
              (let loop ((l lst) (i 0))
                (cases listval l

                  (empty-list-val () 
                    (eopl:error 'ref-list-prim "Index out of bounds: ~s" idx))

                  (list-cons (h t) 
                    (if (= i idx) h (loop t (+ i 1))))))

              ;; Si nos pasaron un indice no entero sacamos error
              (eopl:error 'ref-list-prim "List index must be an integer, got ~s" idx))))

      ;; misma logica que el anterior, pero ahora en vez de devolver el valor, devolvemos una nueva lista con
      ;; el valor reemplazando al head original. 
      (set-list-prim () 
        (let ((lst (car args)) (idx (cadr args)) (val (caddr args)))

          (if (integer-valued? idx)

              (let loop ((l lst) (i 0))
                (cases listval l
                  (empty-list-val () (eopl:error 'set-list-prim "Index out of bounds: ~s" idx))
                  (list-cons (h t) (if (= i idx) (list-cons val t) (list-cons h (loop t (+ i 1)))))))

              (eopl:error 'set-list-prim "List index must be an integer, got ~s" idx))))
      
      ;; DICCIONARIOS

      ;; intuitivo.
      (make-dict-prim ()
        (let loop ((remaining args) (acc '()))
          (if (null? remaining)
              
              (dict-val (reverse acc)) ;; si ya no quedan dict-vals para evaluar, invertimos el acumulador para que quede bien ordenado

              ; sino, entonces obtenemos key - value
              ; si la key no es string / numero lanzamos error. Sino, entonces seguimos construyendo el dic con (cons (key value) acc)
              (let ((key (car remaining)) (val (cadr remaining)))
                (unless (dict-key-value? key)
                  (eopl:error 'make-dict-prim "Dictionary keys must be strings or numbers, got: ~s" key))
                (loop (cddr remaining) (cons (cons key val) acc))))))

      ;; checkea si es el datatype
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
              (letrec 
                ((loop (lambda (ps acc)
                  (if (null? ps)
                      acc
                      (loop (cdr ps) (list-cons (caar ps) acc))))))

                (loop pairs (empty-list-val)))))))

      (values-prim ()
        (let ((dict (car args)))
          (cases dictval dict
            (dict-val (pairs)
              (letrec 
                ((loop (lambda (ps acc)
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

;; simplificacion algebraica basica para expresiones simbolicas

;; simplify-simbolic obtiene la expresion, verifica que es una symexpr con cases
;; y llama de la siguiente forma: simplificar-operacion(operacion, simplificar-operando-izq, simplificar-operando-der) }
;; por ultimo devuelve el resultado o la misma expresion si no era una symexpr
(define simplify-symbolic
  (lambda (v)
    (if (symexpr? v)
        (cases symexpr v
          (symbolic-exp (op lhs rhs)
            (simplify-symbolic-op op (simplify-symbolic lhs) (simplify-symbolic rhs))))

        v)))

; es un wrapper de las funciones de simplificacion para nuestras expresiones algebraicas. Si la operacion es +, -, *, / llama a la funcion de simplificacion correspondiente
; sino pues reconstruimos la expresion algebraica con operador - left hand side - right hand side
; tambien normaliza para que las constantes siempre sean el lado derecho (para suma y multiplicacion solamente, que son conmutativas)
(define simplify-symbolic-op
  (lambda (op lhs rhs)
    (let* ((normalized (normalize-commutative op lhs rhs))
           (lhs (car normalized))
           (rhs (cdr normalized)))
      (case op
        ((+) (simplify-plus lhs rhs))
        ((-) (simplify-minus lhs rhs))
        ((*) (simplify-mult lhs rhs))
        ((/) (simplify-div lhs rhs))
        (else (rebuild-symbolic op lhs rhs))))))

; para comparar si 2 expresiones son iguales. Como tenemos 2 datatypes para expresiones simbolicas,
; tenemos que hacer un condicional para ver si son del mismo tipo y luego comparar sus campos. 
; Si no son del mismo tipo, usamos equal? de racket
(define symbolic-structural-equal?
  (lambda (a b)
    (cond
      ((and (symval? a) (symval? b))
       (cases symval a
         (math-symbol (id-a)
           (cases symval b
             (math-symbol (id-b) (eqv? id-a id-b))))))

      ((and (symexpr? a) (symexpr? b))
       (cases symexpr a
         (symbolic-exp (op-a lhs-a rhs-a)
           (cases symexpr b
             (symbolic-exp (op-b lhs-b rhs-b)
               (and (eq? op-a op-b)
                    (symbolic-structural-equal? lhs-a lhs-b)
                    (symbolic-structural-equal? rhs-a rhs-b)))))))

      (else (equal? a b))))) ; Esto por si las moscas, pero realmente no deberia pasar y va a devolver false xd

(define number-zero?
  (lambda (v)
    (and (number? v) (zero? v))))

(define number-one?
  (lambda (v)
    (and (number? v) (= v 1))))

; para evaluar o hacer la simplificacion de una expresion simbolica cuando solo quedan numeros en la operacion binaria. Sino devolvemos la misma symbolic-exp.
(define rebuild-symbolic
  (lambda (op lhs rhs)
    (if (and (number? lhs) (number? rhs))
        (case op
          ((+) (+ lhs rhs))
          ((-) (- lhs rhs))
          ((*) (* lhs rhs))
          ((/) (/ lhs rhs))

          ((%) (if (and (integer-valued? lhs) (integer-valued? rhs))
                   (modulo lhs rhs)
                   (symbolic-exp op lhs rhs)))

          (else (symbolic-exp op lhs rhs)))

        (symbolic-exp op lhs rhs))))

;; Si teniamos algo del estilo +(2,x), es decir, con la constante como lado izquierdo, lo dejamos en
;; +(x,2). Esto es debido a que nuestro decompose-binary para sumas hace la simplificacion verificando la constante para el lado derecho
(define normalize-commutative
  (lambda (op lhs rhs)
    (if (and (member op '(+ *)) (number? lhs) (not (number? rhs)))
        (cons rhs lhs)  ; swap
        (cons lhs rhs))))

;; si tenemos +(+(x,2),3) => op = +, expr se pasa como alguno de los dos lados
;; por ejemplo, si fuera lhs tendriamos exp = +(x,2)
;; cuando entre a cases las operaciones seran iguales, y al b ser un numero tendriamos una devuelta: 
;; (x 2)
(define decompose-binary-with-num-right
  (lambda (op expr)
    (if (symexpr? expr)
        (cases symexpr expr
          (symbolic-exp (op2 a b)
            (if (and (eq? op op2) (number? b))
                (cons a b)
                #f)))
        #f)))

;; Simplificacion de sumas
(define simplify-plus
  (lambda (lhs rhs)
    (cond
      ((number-zero? lhs) rhs) ; 0 + x = x 
      ((number-zero? rhs) lhs) ; x + 0 = x

      ;; +(+(x,c1),c2) = +(x, +(c1,c2))
      ;; si el lado derecho es un numero
      ;; y el lado izquierdo cumple con ser tambien una suma con el lado derecho siendo un numero
      ;; por ejemplo: lhs = +(x,2)
      ;; Entonces decompose-binary-with-num-right nos devuelve la lista (x 2)
      ;; obtenemos x como el primer elemento, la constante como el segundo y hacemos la suma de las 2 constantes.
      ((and (number? rhs)
            (decompose-binary-with-num-right '+ lhs))
       (let* ((inner (decompose-binary-with-num-right '+ lhs))
              (x (car inner))
              (c1 (cdr inner)))
         (symbolic-exp '+ x (+ c1 rhs))))

      ;; +(c1, +(x,c2)) = +(x, +(c1,c2))
      ;; misma logica que antes, pero ahora con el lado derecho siendo el que descomponemos en lista
      ((and (number? lhs)
            (decompose-binary-with-num-right '+ rhs))
       (let* ((inner (decompose-binary-with-num-right '+ rhs))
              (x (car inner))
              (c1 (cdr inner)))
         (symbolic-exp '+ (symbolic-exp '+ c1 lhs) x)))

      ;; +(-(x,c2), c1) = +(x, +(-c1,c2)), if +(-c1,c2) = 0 then x
      ((and (number? rhs)
            (decompose-binary-with-num-right '- lhs))

        (let* ((inner (decompose-binary-with-num-right '- lhs))
                    (x (car inner))
                    (c1 (cdr inner)))
            (let ((result (+ (- c1) rhs)))  ; -c1 + rhs
                (if (zero? result)
                    x
                    (rebuild-symbolic '+ x result)))))

      (else (rebuild-symbolic '+ lhs rhs)))))

;; restas
(define simplify-minus
  (lambda (lhs rhs)
    (cond
      ((symbolic-structural-equal? lhs rhs) 0) ;; Si el lado derecho e izquierdo son iguales, 0
      
      ;; 0 - x = *(x,-1)
      ((number-zero? lhs)
       (rebuild-symbolic '* -1 rhs))

      ((number-zero? rhs) lhs) ; x - 0 = x

      ;; -(+(x,c2), c1) =  +(x, -(c2,c1))
      ;; misma logica que tenemos para la descomposicion de sumas anidadas, pero ahora con la resta.
      ;; La unica diferencia es que la normalizacion no se aplica a restas porque -(c1,x) != -(x,c1)
      ((and (number? rhs)
            (decompose-binary-with-num-right '+ lhs))

       (let* ((inner (decompose-binary-with-num-right '+ lhs))
              (x (car inner))
              (c1 (cdr inner)))
         (simplify-plus x (- c1 rhs))))

      ;; -(-(x,c2), c1) = -(x, +(c1,c2))
      ;; same thing pero repartiendo la resta
      ((and (number? rhs)
            (decompose-binary-with-num-right '- lhs))

        (let* ((inner (decompose-binary-with-num-right '- lhs))
                (x (car inner))
                (c1 (cdr inner)))
        (rebuild-symbolic '- x (+ c1 rhs))))

      ;; Podriamos meter reglas como -(c1,x) = +(*(x,-1), c1) pero no se que tan "simplificacion" sea

      (else (rebuild-symbolic '- lhs rhs)))))

;; multiplicaciones
(define simplify-mult
  (lambda (lhs rhs)
    (cond
      ((number-zero? lhs) 0) ; 0 * x = 0
      ((number-zero? rhs) 0) ; x * 0 = 0
      ((number-one? lhs) rhs) ; 1 * x = x
      ((number-one? rhs) lhs) ; x * 1 = x

      ; *(*(x,c2), c1) = *(x, *(c1,c2))
      ; usamos la misma logica que para la agrupacion de sumas
      ((and (number? rhs)
            (decompose-binary-with-num-right '* lhs))
       (let* ((inner (decompose-binary-with-num-right '* lhs))
              (x (car inner))
              (c1 (cdr inner)))
         (symbolic-exp '* x (* c1 rhs))))

      ; misma que la anterior pero para *(c1, *(x,c2)), o sea si el numero está en la primera posicion
      ((and (number? lhs)
            (decompose-binary-with-num-right '* rhs))
       (let* ((inner (decompose-binary-with-num-right '* rhs))
              (x (car inner))
              (c1 (cdr inner)))
         (symbolic-exp '* (* c1 lhs) x)))

      (else (rebuild-symbolic '* lhs rhs)))))

;; division 
(define simplify-div
  (lambda (lhs rhs)
    (cond
      ((number-zero? lhs) 0) ; 0 / x = 0

      ((number-zero? rhs) ; x / 0 = error
       (eopl:error 'simplify-div "Division by zero in symbolic simplification: ~s / ~s" lhs rhs))

      ((number-one? rhs) lhs) ; x / 1 = x 

      ((symbolic-structural-equal? lhs rhs) 1) ; x / x = 1

      ; /(*(x,c2),c1) = *(x, /(c2,c1)), if c1 == c2 then x, if c2 == 0 se paraba desde el caso de la division por 0
      ((and (number? rhs)
            (decompose-binary-with-num-right '* lhs))
        (let* ((inner (decompose-binary-with-num-right '* lhs))
                (x (car inner))
                (c1 (cdr inner)))
        (let ((result (/ c1 rhs)))
            (if (= result 1)
                x
                (rebuild-symbolic '* x result)))))

      (else (rebuild-symbolic '/ lhs rhs)))))

;; -------------------------------
;; ------- EVALUACION ------------
;; -------------------------------

;; con la expresion y los bindings llamamos a simplificar con el resultado de la substitucion de la expresion con los bindings
(define evaluate-symbolic
  (lambda (expr bindings)
    (simplify-symbolic (substitute-symbolic expr bindings '()))))

;; auxiliar para buscar en la lista de bindings un par (id value)
(define lookup-binding
  (lambda (id bindings)
    (cond
      ((null? bindings) #f) ; si la lista de bindings está vacia devolvemos false. No está el id en los bindings
      ((eqv? id (caar bindings)) (car bindings)) ; si el id es el el primer elemento del primer binding, devolvemos ese par de binding (id value)
      (else (lookup-binding id (cdr bindings)))))) ; si no, buscamos recursivamente en el resto de los bindings

;; auxiliar para saber si un id está en una lista
(define symbol-in-list?
  (lambda (id lst)
    (cond
      ((null? lst) #f)
      ((eqv? id (car lst)) #t)
      (else (symbol-in-list? id (cdr lst))))))

(define substitute-symbolic
  (lambda (expr bindings seen)
    (cond
      ((symval? expr)
       (cases symval expr
        
         ;; si es un simbolo matematico lo buscamos reemplazar por su valor del binding
         (math-symbol (id)
           (let ((binding (lookup-binding id bindings)))
             (if (and binding
                      (not (symbol-in-list? id seen)))

                    ;; si existe el binding y no está en la lista de vistos, devolvemos la evaluacion de substitucion del valor al que estaba bindeado
                    ;; esto porque decidí que los bindings puedan estar a otros simbolos, entonces por ejemplo si x = y, y luego tendria que ser substituida tambien
                    (substitute-symbolic
                        (cdr binding)
                        bindings
                        (cons id seen))

                    ;; si no estaba en los bindings devolvemos la misma expr. Este puede ser el caso cuando substitute-symbolic es llamado con un numero, como en la parte de arriba
                    expr)))
        ))

      ;; si es una expresion, formamos la expresion de nuevo pero con la evaluacion de substitucion de los operandos izquierdo y derecho :3
      ((symexpr? expr)
       (cases symexpr expr
         (symbolic-exp (op lhs rhs)
           (symbolic-exp op
             (substitute-symbolic lhs bindings seen)
             (substitute-symbolic rhs bindings seen)))))

      ;; si no era devolvemos exp, aunque esto no deberia pasar... Plantearse si deberiamos lanzar error. 
      (else expr))))

; El evaluador. Una de las partes mas interesantes del interprete junto con el manejo de ambientes. Aquí definimos el comportamiento de cada una de las expresiones del lenguaje, y como se van a evaluar. La evaluacion es recursiva, y se hace en el contexto de un ambiente que puede ser modificado por las declaraciones de variables y constantes.

;; el eval-expression
;; Podemos verlo como una función que convierte el AST en valores, propagando los cambios en el ambiente. Para esto, todas las funciones devuelven el par (valor, nuevo-ambiente) para propagar los cambios en el ambiente.
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      ;; literales
      (lit-exp (datum) (make-result datum env))
      (str-exp (s) (make-result (sanitize-string s) env))
      (true-exp () (make-result #t env))
      (false-exp () (make-result #f env))
      (null-exp () (make-result 'null env))
      (empty-list-exp () (make-result (empty-list-val) env))
      
      ;; Declaracion de variables normales y constantes
      (var-decl-exp (id rhs)
                    (let ((res (eval-expression rhs env))) ; res = resultado y ambiente resultante de evaluar lado derecho
                      (let ((v (result-val res)) (env2 (result-env res))) ; v = resultado, env2 = ambiente resultante
                        (if (env-has-symbolic-binding? env2 id)
                            (eopl:error 'eval-expression
                                        "Cannot declare variable ~s: name is already used by a mathematical symbol"
                                        id)
                            (make-result 
                              v 
                              (extend-env (list id) (list (direct-target v)) env2)
                            ))))) ; devolvemos el valor y como ambiente una extension del actual con el nuevo identificador y valor.     
      
      (const-decl-exp (id rhs)
                      (let ((res (eval-expression rhs env)))
                        (let ((v (result-val res)) (env2 (result-env res)))
                          (if (env-has-symbolic-binding? env2 id)
                              (eopl:error 'eval-expression
                                          "Cannot declare constant ~s: name is already used by a mathematical symbol"
                                          id)
                              (make-result v (extend-env (list id) (list (const-target v)) env2))))))
  
      ;; Manejo de creacion de punteros / aliases (var* y = x)
      (var-ref-decl-exp (id rhs-id)
                      (let ((ref (apply-env-ref env rhs-id))) ;; Obtenemos la referencia de la parte derecha
                        (let ((target-val (primitive-deref ref))) ;; Obtenemos el target de la referencia (valor de la posicion en el vector ambiente)
                          (cases target target-val

                            ;; Si era a un direct target, devolvemos el valor y como ambiente una extension con el id del puntero siendo un indirect-target de la referencia con el simbolo rhs-id (x en el ejemplo)
                            (direct-target (expval)
                                          (make-result expval (extend-env (list id) (list (indirect-target ref)) env)))
                            
                            ;; Si era constante devolvemos error. Decidí que el lenguaje no tuviera referencias a constantes.
                            (const-target (expval)
                                        (eopl:error 'eval-expression "Cannot bind ref ~s to const" rhs-id))
                            
                            ;; Si la referencia iba ya de por si a un indirect target (otro ref), entonces devolvemos su desreferenciacion y expandimos el ambiente con nuestro id y un indirect-target a la referencia que contenia nuestro lado derecho (para ahorrarnos ruta).
                            ;; Esto funciona ya que nunca vamos a tener un sistema de referencias circulares: Es una cadena que siempre debe terminar en un direct-target.
                            (indirect-target (ref1)
                                           (make-result (deref ref1) (extend-env (list id) (list (indirect-target ref1)) env)))))))  

      ;; manejo de los identificadores y los casos de sus sufijos. Tenemos 3 casos: asignacion, llamada a funcion y variable simple. 
      ;; la asignacion modifica el ambiente, la llamada a funcion evalua los argumentos y aplica la funcion, y la variable simple devuelve su valor.
      (id-dispatch (id suffix)
                   (cases id-suffix suffix
                     (assign-suffix (rhs)
                                    (let ((res (eval-expression rhs env))) ;; evaluamos el lado derecho de la asignacion
                                      (let ((v (result-val res)) (env2 (result-env res))) ;; v = resultado, env2 = ambiente modificado
                                        (if (symval? (apply-env env2 id))
                                            (eopl:error 'eval-expression
                                                        "Cannot assign to mathematical symbol ~s"
                                                        id)
                                            (begin
                                              (setref! (apply-env-ref env2 id) v) ;; modificamos la referencia del identificador en el ambiente con el nuevo valor
                                              (make-result 1 env2)))))) ;; una asignacion exitosa devuelve 1 y el ambiente modificado

                     (call-suffix (rands)
                                  (let ((proc (apply-env env id)))
                                    (if (procval? proc)

                                        ; si el id() es una funcion, entonces evaluamos los operandos con eval-rands y aplicamos la funcion con apply-procedure y el ambiente resultante de la evaluacion de los operands
                                        (let ((rands-res (eval-rands rands env)))
                                          (let ((args (result-val rands-res)) (env2 (result-env rands-res)))
                                            (apply-procedure proc args env2)))

                                        ; si no dio una funcion cuando lo buscamos en el ambiente, lanzamos error
                                        (eopl:error 'eval-expression "Attempt to call non-procedure ~s" id))))

                     ; Si no habia sufijo simplemente obtenemos el valor del identificador en el ambiente y lo devolvemos
                     (empty-suffix () (make-result (apply-env env id) env))

                     (else (eopl:error 'eval-expression "Unknown id-suffix ~s" suffix))))

      
      ;; aplicacion de primitivas. Evaluamos los operandos y aplicamos la primitiva con apply-primitive
      (primapp-exp (prim rands)
                   (let ((res (eval-primapp-exp-rands rands env)))
                     (let ((args (result-val res)) (env2 (result-env res)))
                       (make-result 
                          (apply-primitive prim args) 
                          env2))))
      
      ;; flujos de control :Z
      (if-exp (test-exp true-exp false-exp)
              (let ((res (eval-expression test-exp env)))
                (let ((v (result-val res)) (env2 (result-env res)))
                  (if (true-value? v)
                      (eval-expression true-exp env2) ; Si v era verdadero, devolvemos la evaluacion de la true exp
                      (eval-expression false-exp env2))))) ; Sino, devolvemos la evalacion de la false y el ambiente resultante
      
      ;; Bloques begin ... end. Hacemos un loop donde en cada paso se evalua una expresion de la lista de expresiones hasta que se acaba la lista de expresiones restantes. 
      ;; Devuelven el resultado del ultimo elemento evaluado para que el return de las funciones funque :3
      (begin-exp (exp exps)
                 (let loop ((res (eval-expression exp env)) (exps exps))
                   (let ((env1 (result-env res)))
                     (if (null? exps)
                         res
                         (loop (eval-expression (car exps) env1) (cdr exps))))))
      
      ;; Switch expression
      ;; Tiene un limitante: Los cases pueden intentar matchear cualquier tipo de valor que no necesariamente sea el mismo que el test.
      (switch-exp (test-exp case-exps default-body)
        (let ((res (eval-expression test-exp env)))
          (let ((test-res (result-val res)) (env2 (result-env res)))
            (if (switch-case-value? test-res)
                (let loop ((cases-list case-exps) (env-curr env2))
                  (if (null? cases-list)
                      (eval-expression (make-body-exp default-body) env-curr)

                      (cases case-clause (car cases-list)
                        (case-clause-exp (match-exp body-exps)
                          (let ((match-res (eval-expression match-exp env-curr)))
                            (let ((match-val (result-val match-res)) (env3 (result-env match-res)))
                              (if (equal? test-res match-val)
                                  
                                  (eval-expression (make-body-exp body-exps) env3)

                                  (loop (cdr cases-list) env3))))))))

                (eopl:error 'eval-expression
                            "switch expects a number, string, boolean or symbol, got: ~s"
                            test-res)))))

      ;; Funciones. Basicamente, crea un body que luego es wrappeado en un closure.
      (func-exp (name params body-exps f-return)
                (if (env-has-symbolic-binding? env name)
                    (eopl:error 'eval-expression
                                "Cannot declare function ~s: name is already used by a mathematical symbol"
                                name)
                    (let* ((body (cases func-return f-return
                                   (func-return-exp (return-exp) ;; Si tenemos un return explicito, lo juntamos como la ultima instruccion en el body que es simplemente una begin-exp
                                     (if (null? body-exps)
                                         return-exp
                                         (begin-exp (car body-exps) (append (cdr body-exps) (list return-exp)))))
                                   (empty-return-exp () ;; Juntamos una null-exp al final del body si no habia return
                                     (if (null? body-exps)
                                         (null-exp)
                                         (begin-exp (car body-exps) (append (cdr body-exps) (list (null-exp))))))))
                          (vec (make-vector 1)) 
                          (recursive-env (extended-env-record (list name) vec env)) ;; creamos el ambiente recursivo antes del closure para que la funcion se pueda encontrar a si misma si usa recursion
                          (proc (closure params body recursive-env))) ;; proc = closure de la funcion creado
                      (vector-set! vec 0 (direct-target proc)) ;; modificamos el vector del ambiente con el procedimiento ya hecho
                      (make-result proc recursive-env)))) ;; Devolvemos el closure de la funcion y el ambiente extendido creado que lo contiene a si mismo

      ;; call-exp. Creada por un intento de implementacion de objetos usando funciones con estado.
      ;; Antes habia que si o si usar un identificador para poder llamar una funcion. 
      ;; Ahora podemos usar esta forma para evaluar una expresion que devuelve una funcion anonima
      (call-exp (func-expr rands)
        (let ((res (eval-expression func-expr env)))
          (let ((proc (result-val res)) (env2 (result-env res)))
            (if (procval? proc)
                (let ((rands-res (eval-rands rands env2)))
                  (let ((args (result-val rands-res)) (env3 (result-env rands-res)))
                    (apply-procedure proc args env3)))

                ;; Si lo que devolvio la expresion no era un procedimiento entonces lanzamos error
                (eopl:error 'eval-expression "Attempt to call non-procedure ~s" func-expr)
            )
          )
        )
      )

      ; Estructuras de ciclos

      ;; while
      (while-exp (cond-exp body-exp)
                 (let loop ((env-curr env)) ;; env-curr = ambiente actual del loop

                   (let ((cond-res (eval-expression cond-exp env-curr))) ;; cond-res = resultado de evaluar la condicion
                     (let ((cond-val (result-val cond-res)) (env1 (result-env cond-res))) 
                       (if (true-value? cond-val) 

                           ;; Si la condicion es verdadera, entonces volvemos a ejecutar el loop con el ambiente resultante de haber evaluado el cuerpo 
                           (let ((body-res (eval-expression body-exp env1)))
                             (loop (result-env body-res)))

                           ;; While no devuelve nada en valor (más si el ambiente modificado), solo realiza las operaciones dentro de el hasta que la condicion acaba
                           (make-result 'null env1))))
                  ))
      
      ;; for de la forma for id in list do ... done
      (for-exp (id list-exp body-exp)
         (let ((list-res (eval-expression list-exp env)))
           (let ((list-val (result-val list-res)) (env1 (result-env list-res)))
             (if (listval? list-val)
                 (let loop ((lst list-val) (env-curr env1))
                   (cases listval lst
                     (empty-list-val () (make-result 'null env-curr))
                     (list-cons (item tail)
                       (let ((body-env (extend-env (list id) (list (direct-target item)) env-curr)))
                         (let ((body-res (eval-expression body-exp body-env)))
                           (loop tail (result-env body-res)))))))
                 (eopl:error 'eval-expression "for expects a list, got ~s" list-val)))))
      
      ;; evaluacion de expresiones en una lista que devuelve la lista con los resultados evaluados y el nuevo ambiente despues de haberlo hecho
      (list-exp (exps)
          (let loop ((es (reverse exps)) (acc (empty-list-val)) (env-curr env))
            (if (null? es)
                (make-result acc env-curr)
                (let ((e-res (eval-expression (car es) env-curr)))
                  (loop (cdr es) (list-cons (result-val e-res) acc) (result-env e-res))))))
      
      ;; evaluacion de una expresion de diccionario que usa como auxiliar a eval-dict-pair para cada pareja :). Hace que la estructura de diccionario tenga como identificador 'dict al inicio.
      (dict-exp (pairs)
        (let loop ((ps pairs) (acc '()) (env-curr env))
          (if (null? ps)
              (make-result (dict-val (reverse acc)) env-curr)
              (let ((pair-res (eval-dict-pair (car ps) env-curr)))
                (loop (cdr ps) (cons (result-val pair-res) acc) (result-env pair-res))))))
      
      ;; por si la persona intenta hacer algo como "ref x" o algo del estilo por fuera de un call a funcion :x
      (ref-exp (id)
               (eopl:error 'eval-expression
                           "ref x\' Only valid as an argument in function calls, not as an independent expression: ~s"
                           id))

      ;; todo el tema de manejo algebraico. No está terminado, asi que lo deje como dummies por ahora xd
      (symbol-exp (id)
                  (if (env-has-binding? env id)
                      (eopl:error 'eval-expression
                                  "Cannot declare symbol ~s: identifier is already bound"
                                  id)
                      (let ((sym (math-symbol id)))
                        (make-result
                         sym
                         (extend-env (list id) (list (direct-target sym)) env)))) )
      
      (simplify-exp (expr)
                    (let ((res (eval-expression expr env)))
                      (let ((v (result-val res)) (env2 (result-env res)))
                        (if (symbolic-expval? v)
                            (make-result (simplify-symbolic v) env2)
                            (eopl:error 'simplify-exp
                                        "simplificar expects a symbolic expression, got: ~s"
                                        v)))))
      
      (evaluate-exp (expr bindings)
                    (let ((res (eval-expression expr env)))
                      (let ((v (result-val res)) (env2 (result-env res)))
                        (if (symbolic-expval? v)
                            (let ((bindings-res (eval-evaluate-bindings bindings env2)))
                              (let ((bindings-alist (result-val bindings-res)) (env3 (result-env bindings-res)))
                                (make-result (evaluate-symbolic v bindings-alist) env3)))
                            (eopl:error 'evaluate-exp
                                        "evaluar expects a symbolic expression, got: ~s"
                                        v)))))
      
      ;; Por si nos pasa alguito
      (else (eopl:error 'eval-expression "Expression case not yet implemented: ~s" exp)))))

;; Funciones de ayuda para evaluar operandos / argumentos de funciones

;; Evalua un operando simple. Si se pasa un identificador normal o una expresion usa valor, si se pasa un ref id se usa paso por referencia.
(define eval-rand
  (lambda (rand env)
    (cases expression rand

      ; cuando es una referencia (por ejemplo, multiplicarx2(ref y))
      (ref-exp (id)
               (let ((ref (apply-env-ref env id))) ;; ref = referencia obtenida con el ambiente actual y el id
                 (let loop ((ref-to-direct ref)) ;; si usamos ref de un indirect-target, tenemos que buscar hasta encontrar el direct-target asociado
                   (let ((target-val (primitive-deref ref-to-direct)))
                     (cases target target-val
                       (direct-target (expval)
                                     (make-result (indirect-target ref-to-direct) env))
                       (const-target (expval)
                                     (eopl:error 'eval-rand "Cannot create reference to const ~s" id))
                       (indirect-target (ref1)
                                        (loop ref1))))))) ;; Si era un indirect-target, seguimos el loop hasta que lleguemos a nuestro direct

      ; si es un identificador entonces devolvemos su valor de la forma (direct-target (deref ref))
      (id-dispatch (id suffix)
                   (cases id-suffix suffix
                     (empty-suffix ()
                                   (let ((ref (apply-env-ref env id)))
                                     (make-result
                                      (direct-target (deref ref))
                                      env)))

                     ; Si ponemos cualquier cosa que no fuera un identificador, lanzamos error
                     (else (eopl:error 'eval-rand "Invalid identifier form in argument: ~s" rand))))

      ; si no es un identificador entonces evaluamos la expresión y devolvemos el target directo con el valor resultante
      (else
       (let ((res (eval-expression rand env)))
         (make-result 
            (direct-target (result-val res)) 
            (result-env res)))))))

;; eval-rands 
;; helper para evaluar todos los operandos de una llamada a funcion, devolviendo la lista de valores y el ambiente final.
;; OJO: cada argumento se envuelve en un target en eval-rand (direct-target para valores normales
;; y indirect-target para referencias). Esto es el invariante que apply-procedure espera.
(define eval-rands
  (lambda (rands env)
    (let loop ((rs rands) (acc '()) (env env))
      (if (null? rs)
          (make-result (reverse acc) env) ;; usamos reverse porque se va formando con cons, que hace que el orden quede inverso 

          (let ((res (eval-rand (car rs) env)))
            (loop (cdr rs) (cons (result-val res) acc) (result-env res)))))))

;; evaluador de parejas en diccionario
(define eval-dict-pair
  (lambda (pair env)
    (cases dict-pair pair
      (pair-exp (key value-exp)
                (let ((key-res (eval-expression key env)))
                  (let ((key-val (result-val key-res)) (env2 (result-env key-res)))
                    (if (dict-key-value? key-val)
                        (let ((val-res (eval-expression value-exp env2)))
                          (make-result (cons key-val (result-val val-res)) (result-env val-res)))

                        (eopl:error 'eval-dict-pair "Dictionary keys must evaluate to strings or numbers, got: ~s" key-val))))))))

;; evalua la lista de bindings de evaluar y devuelve una alist de symbol -> expval
(define eval-evaluate-bindings
  (lambda (bindings env)
    (let loop ((bs bindings) (acc '()) (env-curr env))
      (if (null? bs)
          (make-result (reverse acc) env-curr)
          (cases binding (car bs)
            (binding-exp (id expr)
              (let ((res (eval-expression expr env-curr)))
                (loop (cdr bs)
                      (cons (cons id (result-val res)) acc)
                      (result-env res)))))))))

;; Evaluador de operandos para primitivas (no hay reference wrapping)
(define eval-primapp-exp-rands
  (lambda (rands env)
    (let loop ((rs rands) (acc '()) (env env))
      (if (null? rs)
          (make-result (reverse acc) env)
          (let ((res (eval-expression (car rs) env)))
            (loop (cdr rs) (cons (result-val res) acc) (result-env res)))))))

;; apply-procedure
;; ejecuta el body del closure obteniendo los argumentos del ambiente donde fue llamado.
;; OJO: args deben ser targets (direct-target / indirect-target), tal como produce eval-rands.
;; Devolvemos el ambiente a nivel del caller. Para entrar mas en detalle mirar el test case de referencias #3
(define apply-procedure
  (lambda (proc args caller-env)
    (cases procval proc
      (closure (ids body env)
               (let ((res (eval-expression body (extend-env ids args env))))
                 (make-result (result-val res) caller-env))))))  
;; verdadero o falso pa
(define true-value?
  (lambda (x)
    (not (or (eq? x 'null) (equal? x 0) (equal? x "") (equal? x #f)))))

;; auxiliar para sanitizar los strings para evitarnos el problema de tener "\"string\""
;; lo que hace es borrar el primer y ultimo caracter del string :)
(define sanitize-string
  (lambda (s)
    (substring s 1 (- (string-length s) 1))))

;; auxiliar para switch. Construye begin-exps con las expresiones dentro de los cases y default
(define make-body-exp
  (lambda (exps)
    (if (null? (cdr exps))
        (car exps)
        (begin-exp (car exps) (cdr exps)))))

; Frontend

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

;; corremos el interpretador
(interpretador)

