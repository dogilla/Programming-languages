#lang plai

{fun {(x : number) (y : boolean)} : number x}
_

{fun {(x: number)(y:number)} : number {+ x y}}

(funT (list (numberT)(booleanT)(numberT)))

_

(parse '{fun {(x : number)} : number {+ x x}})
(parse '{with {{x : number 5} {y : boolean #t}} {+ x y}})
(typeof (parse '{with {{x : number 5} {y : boolean #t} {w : boolean 5}} {+ x y w}}) (phi))
(typeof (parse '{with {{x : number 5} {y : number 10}} {+ x y}}) (phi))
(typeof (parse '{with {{x : boolean #t} {y : boolean #f}} {+ x y}}) (phi))
(typeof (parse '{with {{x : boolean #t} {y : boolean #f}} {or x y}}) (phi))
(prueba '{fun {(x : number)(y : number)} : number {or x y}})
(prueba '{app {fun {(x : number)(y : number)} : number {+ x y}} {#t 3}})

;; pruebas del contexto
(typeof (parse 'x) (phi))
(typeof (parse 'x) (gamma 'x (booleanT) (phi)))
(typeof (parse 'y) (gamma 'x (booleanT) (gamma 'y (numberT) (phi))))
