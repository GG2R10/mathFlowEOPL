#lang eopl
(require "mathflow-types.scm")
(require "mathflow-env.scm")
(require "mathflow-primitives.scm")
(require "mathflow-grammar.scm")

; El evaluador. Una de las partes mas interesantes del interprete junto con el manejo de ambientes. Aquí definimos el comportamiento de cada una de las expresiones del lenguaje, y como se van a evaluar. La evaluacion es recursiva, y se hace en el contexto de un ambiente que puede ser modificado por las declaraciones de variables y constantes.

;; el eval-expression
;; Podemos verlo como una función que convierte el AST en valores, propagando los cambios en el ambiente. Para esto, todas las funciones devuelven el par (valor, nuevo-ambiente) para propagar los cambios en el ambiente.
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      ;; literales
      (lit-exp (datum) (make-result datum env))
      (str-exp (s) (make-result s env))
      (true-exp () (make-result #t env))
      (false-exp () (make-result #f env))
      (null-exp () (make-result 'null env))
      (empty-list-exp () (make-result (vector) env))
      
      ;; Declaracion de variables normales y constantes
      (var-decl-exp (id rhs)
                    (let ((res (eval-expression rhs env))) ; res = resultado y ambiente resultante de evaluar lado derecho
                      (let ((v (result-val res)) (env2 (result-env res))) ; v = resultado, env2 = ambiente resultante
                        (make-result 
                          v 
                          (extend-env (list id) (list (direct-target v)) env2)
                        )))) ; devolvemos el valor y como ambiente una extension del actual con el nuevo identificador y valor.     
      
      (const-decl-exp (id rhs)
                      (let ((res (eval-expression rhs env)))
                        (let ((v (result-val res)) (env2 (result-env res)))
                          (make-result v (extend-env (list id) (list (const-target v)) env2)))))
  
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
                                        (setref! (apply-env-ref env2 id) v) ;; modificamos la referencia del identificador en el ambiente con el nuevo valor
                                        (make-result 1 env2)))) ;; una asignacion exitosa devuelve 1 y el ambiente modificado

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
      
      ;; agrupación de expresiones. 
      (group-exp (exp)
                 (eval-expression exp env))
      
      ;; Bloques begin ... end. Hacemos un loop donde en cada paso se evalua una expresion de la lista de expresiones hasta que se acaba la lista de expresiones restantes. 
      (begin-exp (exp exps)
                 (let loop ((res (eval-expression exp env)) (exps exps))
                   (let ((env1 (result-env res)))
                     (if (null? exps)
                         res
                         (loop (eval-expression (car exps) env1) (cdr exps))))))
      

      
      ;; declaracion de funciones
      (func-exp (name params body-exps return-exp)
                (let* ((body (if (null? body-exps)
                                return-exp ;; Si el body era vacio, hacemos al body la expresion del return
                                (begin-exp (car body-exps) (append (cdr body-exps) (list return-exp))))) ;; Sino, lo hacemos el bloque de expreiones del body y return
                       (proc (closure params body env))) ;; proc = closure de la funcion creado
                  (let ((recursive-env (extend-env (list name) (list (direct-target proc)) env))) ;; Creamos el ambiente recursivo para la funcion que contiene el closure de la misma
                    (make-result proc recursive-env)))) ;; Devolvemos el closure de la funcion y el ambiente extendido creado que lo contiene a si mismo
      
      ;; Siento que esto falla... No le estamos pasando el ambiente recursivo al closure / proc :c

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
      ;; esto está preimplementado para suponer que la lista es un vector. Hay que implementarlo despues XD
      (for-exp (id list-exp body-exp)

               (let ((list-res (eval-expression list-exp env)))
                 (let ((list-val (result-val list-res)) (env1 (result-env list-res)))

                   (if (vector? list-val)
                       ;; si es un vector (lista en nuestro interprete)
                       (let loop ((i 0) (env-curr env1))
                         (if (>= i (vector-length list-val))

                             (make-result 'null env-curr) ;; Si i llega al limite de la lista, terminamos el loop

                             ;; si no
                             (let ((item (vector-ref list-val i))) ;; Obtenemos el valor de la posicion "i" en la lista a iterar
                               (let ((body-env (extend-env (list id) (list (direct-target item)) env-curr))) ;; nuevo ambiente con "id" siendo el item de la lista y extendido del actual
                                 (let ((body-res (eval-expression body-exp body-env))) ;; Evaluamos el body del for con las expresiones que hayan en él y el ambiente creado anteriormente
                                   (loop (+ i 1) (result-env body-res))))))) ;; Hacemos el loop de nuevo pero con i+1

                       ;; Si la expresion por la que iteramos no era una lista, sacamos error
                       (eopl:error 'eval-expression "for expects a list/vector, got ~s" list-val)))))
      
      ;; [ expr1, expr2, ... ]
      (list-exp (exps)
                (let loop ((es exps) (acc '()) (env-curr env))
                  (if (null? es)
                      (make-result (list->vector (reverse acc)) env-curr)
                      (let ((e-res (eval-expression (car es) env-curr)))
                        (loop (cdr es) (cons (result-val e-res) acc) (result-env e-res))))))
      
      ;; { id: expr, ... }
      ;; Aun no se me ha ocurrido como hacer esto XD
      (dict-exp (pairs)
        make-result (vector) env)
      )
      
      ;; todo el tema de manejo algebraico. No está terminado, asi que lo deje como dummies por ahora xd
      (symbol-exp (id)
                  (make-result id env))
      
      (ref-exp (id)
               (eopl:error 'eval-expression "ref expression is only valid as a function argument: ~s" id))
      
      (simplify-exp (expr)
                    (let ((res (eval-expression expr env)))
                      ;; ok buddy
                      res))
      
      (evaluate-exp (expr bindings)
                    (let ((res (eval-expression expr env)))
                      ;; ok buddy don't do nothing lol
                      ;; TODO: hay que manejar bien ese temita de las listas de bindings cuando vamos a evaluar
                      res))
      
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
      (pair-exp (key-id value-exp)
                (let ((val-res (eval-expression value-exp env)))
                  (make-result (cons key-id (result-val val-res)) (result-env val-res)))))))

;; Evaluador de operandos para primitivas (no hay reference wrapping)
(define eval-primapp-exp-rands
  (lambda (rands env)
    (let loop ((rs rands) (acc '()) (env env))
      (if (null? rs)
          (make-result (reverse acc) env)
          (let ((res (eval-expression (car rs) env)))
            (loop (cdr rs) (cons (result-val res) acc) (result-env res)))))))

;; apply-procedure
;; ejecuta el body del closure obteniendo los argumentos del ambiente donde fue llamado
(define apply-procedure
  (lambda (proc args caller-env)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

;; verdadero o falso pa
(define true-value?
  (lambda (x)
    (not (or (eq? x 'null) (equal? x 0) (equal? x "") (equal? x #f)))))

;; Export para los demas modulos (el main solamente creo xd)
(provide
    (all-defined-out)
    ; Aqui los excepts
)

