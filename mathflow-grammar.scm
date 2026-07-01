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

;; export para otros modulos
(provide
    (all-defined-out)
    ; Aqui los excepts
)
