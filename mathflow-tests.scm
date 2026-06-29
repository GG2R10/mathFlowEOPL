#lang eopl
(require "mathflow-frontend.scm")

;; variables y constantes
(display "=== Básicos ===")
(newline)

(display (run "
begin
  var a = 10;
  const b = 20;
  +(a, b)
end
"))       
;; => 30

;; reasignacion de variable
(display (run "
begin
  var n = 5;
  n = 99;
  n
end
"))       
;; => 99

;; Constante no modificable (debe lanzar error)
;; Descomentar para probar el error
; (display (run "
; begin
;   const c = 42;
;   c = 0
; end
; "))

(display "=== Funciones y Recursividad ===")
(newline)

;; Factorial recursivo
(display (run "
begin
  func factorial(n) {
    return if ==(n, 0) then 1 else *(n, factorial(-(n, 1))) end
  } ;

  factorial(5)
end
"))       
;; => 120

;; Fibonacci
(display (run "
begin
  func fib(n) {
    return if <(n, 2) then n else +(fib(-(n,1)), fib(-(n,2))) end
  };
  fib(10)
end
"))       
;; => 55

(display "=== Referencias ===")
(newline)

;; puntero / alias basico
(display (run "
begin
  var x = 10;
  var* y = x;
  y = 99;
  x
end
"))       
;; => 99

;; cadena de punteros
(display (run "
begin
  var x = 1;
  var* y = x;
  var* z = y;
  z = 42;
  x
end
"))       
;; => 42

;; paso por referencia a funcion 
(display (run "
begin
  func duplicar(n) {
    return n = *(n, 2)
  };
  var v = 5;
  duplicar(ref v);
  v
end
"))       

;; Esto fue super interesante de debuguear. En nuestro codigo original de apply-procedure al final de la aplicación
;; devolviamos el ambiente extendido de la función, por lo que al intentar buscar v el apply-env claramente no lo encontraba. 
;; la solución fue evaluar el cuerpo de la funcion con el ambiente del closure, pero no devolver su ambiente resultante sino el del mismo caller:
;; (resultado de evaluar con el ambiente antiguo del closure, ambiente del caller a la función)

;; => 10

(display "=== Listas ===")
(newline)

;; operaciones basicas
(display (run "
begin
  var L = [10, 20, 30];
  cabeza(L)
end
"))       
;; => 10

;; mutacion de lista
(display (run "
begin
  var L = [1, 2, 3];
  L = set-list(L, 1, 99);
  ref-list(L, 1)
end
"))       
;; => 99

;; for sobre una lista
(display (run "
begin
  var L = [1, 2, 3, 4, 5];
  var suma = 0;
  for item in L do
    suma = +(suma, item)
  done;
  suma
end
"))       
;; => 15

(display "=== Diccionarios ===")
(newline)

;; creacion
(display (run "
begin
  var d = {nombre: \"Juan\", edad: 25};
  ref-diccionario(d, \"edad\")
end
"))       
;; => 25

(display "=== While ===")
(newline)

;; suma con while
(display (run "
begin
  var i = 0;
  var acc = 0;
  while <(i, 10) do
    begin
      acc = +(acc, i);
      i = add1(i)
    end
  done;
  acc
end
"))       
;; => 45

(display "=== Casos Borde ===")
(newline)

;; numero negativo
(display (run "-5"))       
;; => -5

;; resta con negativo (que miedo)
(display (run "-(10, -3)"))       
;; => 13

;; true-value? con distintos falsy values
(display (run "
begin
  var r1 = if 0 then 1 else 2 end;
  var r2 = if \"\" then 1 else 2 end;
  var r3 = if null then 1 else 2 end;
  +(+(r1, r2), r3)
end
"))       
;; => 6

;; diccionario vacio y lista vacia (TODO: problema que tenemos, se reconcen iguales) 
(display (run "
begin
  var d = {};
  lista?(d)
end
"))       
;; => true

(display "=== Tests Adicionales ===")
(newline)

(display (run "number?(42)"))       
;; => true

(display (run "string?(\"hola\")"))       
;; => true

(display (run "boolean?(#t)"))       
;; => true

;; prueba de condicionales anidados
(display (run "
begin
  var x = 10;
  if >(x, 5) then
    if <(x, 15) then
      100
    else
      200
    end
  else
    300
  end
end
"))       
;; => 100

;; funciones de orden superior
(display (run "
begin
  func aplicar(f, x) {
    f(x)
  };
  
  func cuadrado(n) {
    *(n, n)
  };
  
  aplicar(cuadrado, 5)
end
"))       
;; => 25
