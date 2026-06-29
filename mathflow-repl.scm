#lang eopl
(require "mathflow-frontend.scm")

; corre el REPL. No mucho mas que decir XD

(display (run "begin

var x = [1,2,3] ;

func testlistasreferencia(x) {
    return print(x = cola(x))
} ;

testlistasreferencia(ref x); 

print(x)

end"))

(interpretador)