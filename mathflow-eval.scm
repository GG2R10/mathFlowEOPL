#lang eopl
(require "mathflow-types.scm")
(require "mathflow-env.scm")
(require "mathflow-primitives.scm")
(require "mathflow-algebraic.scm")
(require "mathflow-grammar.scm")

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

;; Export para los demas modulos (el main y test solamente)
(provide
    (all-defined-out)
    ; Aqui los excepts
)

