#lang plai

{fun {(x : number) (y : boolean)} : number x}
_

{fun {(x: number)(y:number)} : number {+ x y}}
(funT (list (numberT)(booleanT)(numberT)))

_

