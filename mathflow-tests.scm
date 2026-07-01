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
")) (newline)    
;; => 30

;; reasignacion de variable
(display (run "
begin
  var n = 5;
  n = 99;
  n
end
")) (newline)      
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
")) (newline)      
;; => 120

;; Fibonacci
(display (run "
begin
  func fib(n) {
    return if <(n, 2) then n else +(fib(-(n,1)), fib(-(n,2))) end
  };
  fib(10)
end
")) (newline)      
;; => 55

;; funcion sin return => devuelve null
(display (run "
begin
  func sinReturn() {
    var x = 10;
    x = +(x, 5)
  };
  sinReturn()
end
")) (newline)
;; => null

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
")) (newline)       
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
")) (newline)      
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
")) (newline)      

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
")) (newline)      
;; => 10

;; mutacion de lista
(display (run "
begin
  var L = [1, 2, 3];
  L = set-list(L, 1, 99);
  ref-list(L, 1)
end
")) (newline)      
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
")) (newline)      
;; => 15

(display "=== Diccionarios ===")
(newline)

;; creacion
(display (run "
begin
  var nombre = \"Jessica\";
  var d = {nombre: \"Juan\", \"edad\": 25};
  ref-diccionario(d, nombre)
end
")) (newline)       
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
")) (newline)       
;; => 45

(display "=== MathFlow (expresiones simbolicas y simplificar / evaluar) ===")
(newline)

; Deberia de fallar porque ya hay un simbolo en el ambiente

;(display (run "begin
;  symbol simbolito;
;  func simbolito() {print(\"Hola desde la funcion x\")}
; end"))  

(run "
begin
  symbol s;
  var x = 10;
  
  var exp = +(x,15);
  var exp2 = +(s, exp);

  print(exp2)
end
") ; => +(s,25)

(display "=== Simplificar ===")
(newline)

;; +(-(s,2), 5) => +(s,3)
(run "
begin
  symbol s;
  var e1 = simplificar(+(-(s, 2), 5));
  print(e1)
end
")

;; -(s,s) => 0
(run "
begin
  symbol s;
  var e2 = simplificar(-(s, s));
  print(e2)
end
")

;; /(*(s,4),4) => s
(run "
begin
  symbol s;
  var e3 = simplificar(/(*(s, 4), 4));
  print(e3)
end
")

;; *(s,0) => 0
(run "
begin
  symbol s;
  var e4 = simplificar(*(s, 0));
  print(e4)
end
")

;; *(s,1) => s
(run "
begin
  symbol s;
  var e5 = simplificar(*(s, 1));
  print(e5)
end
")

;; +(s,0) => s
(run "
begin
  symbol s;
  var e6 = simplificar(+(s, 0));
  print(e6)
end
")

;; *(*(s,3),2) => *(s,6)
(run "
begin
  symbol s;
  var e7 = simplificar(*(*(s, 3), 2));
  print(e7)
end
")

;; +(+(3,s),4) => +(s,7)
(run "
begin
  symbol s;
  var e8 = simplificar(+(+(3,s), 4));
  print(e8)
end
")

;; -(+(s,5),5) => s
(run "
begin
  symbol s;
  var e9 = simplificar(-(+(s, 5), 5));
  print(e9)
end
")

;; /(*(s,4),2) => *(s,2)
(run "
begin
  symbol s;
  var e9 = simplificar(/(*(s,4),2));
  print(e9)
end
")

(display "=== Evaluar ===")
(newline)

(display (run "
begin
  symbol s;
  evaluar(+(s,2), s=4)
end
")) (newline) ; => 6

(display (run "
begin
  symbol s;
  symbol t;
  evaluar(+(s,*(t,2)), s=2)
end
")) (newline) ; => +(2,*(y,2))

(run "
begin
  symbol s;
  symbol t;
  var x = evaluar(+(s,*(t,2)), h=2, t=3);
  var* y = x;
  print(y)
end
") ; => +(2,*(y,2))

(display "=== Casos Borde ===")
(newline)

;; numero negativo
(display (run "-5")) (newline)       
;; => -5

;; resta con negativo (que miedo)
(display (run "-(10, -3)")) (newline)      
;; => 13

;; true-value? con distintos falsy values
(display (run "
begin
  var r1 = if 0 then 1 else 2 end;
  var r2 = if \"\" then 1 else 2 end;
  var r3 = if null then 1 else 2 end;
  +(+(r1, r2), r3)
end
")) (newline)       
;; => 6

;; diccionario vacio y lista vacia se deberian reconocer distintos 
(display (run "
begin
  var d = {};
  lista?(d)
end
")) (newline)      
;; => false

(display "=== Tests Adicionales ===")
(newline)

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
")) (newline)      
;; => 100

;; funciones de orden superior
(display (run "
begin
  func aplicar(f, x) {
    return f(x)
  };
  
  func cuadrado(n) {
    return *(n, n)
  };
  
  aplicar(cuadrado, 5)
end
")) (newline)   

;; => 25

;; numeros decimales

(display (run "
begin
  var x = 1.1;
  var y = +(x,2.4);

  var lista = [\"tutaina\", 2, y];
  ref-list(lista, 1)
end
")) (newline)

;; alcance lexico con funciones
(run "begin

var x = 1;

func pruebaalcanceambientes(){
    var x = 2;
    x = 3;
    print(x)  
};

pruebaalcanceambientes();
print(x);

print(\"Esto funciona bien porque nuestro ambiente se ejecuta sobre su propia copia del anterior, pero el retorno extiende al ambiente del caller.\");
print(\"Pero si hicieramos una asignacion, si cambiara a x\");

func pruebaalcanceambientes2(){
    x = 99
};

pruebaalcanceambientes2();
print(x)

end")

(run "begin

  func hola(){
    print(\"Hola\")
  };
  
  var x = 1;

  func switchtest(y){
    switch y {
      case 1 {print(\"Caso 1\")}
      case 2 {hola}
      default {print(\"Caso default\")}
    }
  };

  switchtest(x)

end")

; simulacion de objetos por medio de funciones con estado
; Anotacion random: El problema de realizar esto (ademas de practicidad) es que copiamos constantemente todas las clausulas o funciones
; por cada objeto que creamos. Muy poco optimo. 
(run "begin
func Persona(nomb, ed){
    var edad = ed;
    var nombre = nomb;

    func getNombre() {
        return nombre
    }; 

    func getEdad() {
        return edad
    };

    func setEdad(nuevaEdad) {
        edad = nuevaEdad
    };

    func setNombre(nuevoNombre) {
        nombre = nuevoNombre
    };

    func dispatch(metodo) {
        print(metodo)
        
        return switch metodo {
            case \"getNombre\" {getNombre}
            case \"getEdad\" {getEdad}
            case \"setEdad\" {setEdad}
            case \"setNombre\" {setNombre}
            default { print(\"Metodo no encontrado\") }
        }
    }

    return dispatch 
};

func Estudiante(nom, ed, cod, car){

  var codigo = cod;
  var carrera = car;
  var persona = Persona(nom, ed);

  func getCodigo() {
      return codigo
  };

  func getCarrera() {
      return carrera
  };

  func dispatch(metodo) {
      return switch metodo {
          case \"getCodigo\" {getCodigo}
          case \"getCarrera\" {getCarrera}
          default { persona(metodo) }
      }
  }

  return dispatch
};

var p = Persona(\"Juan\", 30);
print((p(\"getNombre\"))());

var e = Estudiante(\"Maria\", 20, \"12345\", \"Ingenieria\");
print((e(\"getNombre\"))());
print((e(\"getCarrera\"))())

end")
