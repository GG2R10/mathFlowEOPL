#lang eopl
(require "mathflow-types.scm")

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

;; mejoramos las 3 funciones de rib-find-position a solo list-index, que es usada para buscar la posicion del simbolo en la lista de simbolos en apply-env-ref
(define list-index
  (lambda (pred ls)
    (let loop ((ls ls) (n 0))
      (cond
        ((null? ls) #f)
        ((pred (car ls)) n)
        (else (loop (cdr ls) (+ n 1)))))))

;; nombre para ambiente inicial. Tiene algunos direct targets creados
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     (list (direct-target 1)
           (direct-target 5)
           (direct-target 10))
     (empty-env))))

;; El export para los otros modulos
(provide
    (all-defined-out)
    ; Aqui los excepts
)

