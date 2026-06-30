#lang eopl
(require "mathflow-frontend.scm")

; corre el REPL. No mucho mas que decir XD
; hay algunas pruebas random

(display (run "begin

var x = [1,2,3] ;

func testlistasreferencia(x) {
    return print(x = cola(x))
} ;

testlistasreferencia(ref x); 

print(x)

end"))
(display "\n")

(display (run "begin

func testlistasreferencia(x) {
    return print(x = cola(x))
} ;

var diccio = crear-diccionario(\"Ana\", 10, \"Juan\", 20, 35.40, \"float\"); 

var listacursed = [1, \"string\", func wtf() {return 42}, diccio, if ==(1, 1) then 100 else 200 end];
var h = ref-list(listacursed, 4)
end"))

(display "\n")

(display (run "begin

var neg = -5;

func testnegativo(x) {
    print(x = +(x, -(-1,-7)))
} ;

testnegativo(neg)

end"))

(display "\n")

(interpretador)
