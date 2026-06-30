#lang eopl
(require "mathflow-frontend.scm")

; corre el REPL. No mucho mas que decir XD
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
    }

    return {\"getNombre\": getNombre, \"getEdad\": getEdad, \"setEdad\": setEdad, \"setNombre\": setNombre}
};

var p = Persona(\"Juan\", 30);

print((ref-diccionario(p, \"getNombre\"))());
(ref-diccionario(p, \"setNombre\"))(\"Peter\");
print((ref-diccionario(p, \"getNombre\"))())

end")

(run "begin
    var lista1 = [1,2,3];
    var lista2 = [4,5,6];
    print(append(lista1, lista2))
end")

(interpretador)
