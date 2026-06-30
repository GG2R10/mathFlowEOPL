#lang eopl
(require "mathflow-types.scm")

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

(provide
    (all-defined-out))
