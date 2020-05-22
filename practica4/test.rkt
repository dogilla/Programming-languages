#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./interp.rkt"))

(define (prueba sexp)
  (interp (parse sexp) (mtSub)))

(test (prueba '{with {{x 5}
                       {w {+ x 1}}
                       {z {with {{x 10}
                                 {f {fun {a} {+ x a}}}}
                                {f {10}}}}}
                       {+ x z}}) (numV 25))



(test (prueba '{with {{x 3}}
                      {with* {{f {fun {y} {+ x y}}}}
                             {with* {{x 4}}
                                    {f {1}}}}}) (numV 4)) 

(test (prueba '{with {{x 5} {y 1}} {+ x y}}) (numV 6))

(test (prueba '{with* {{x 5} {y 1}} {+ x y}}) (numV 6)) 

(test (prueba '{with* {{x 5} {y {+ x 1}}} {+ x y}}) (numV 11))

(test/exn (prueba '{with {{x 5} {y {+ x 1}}} {+ x y}}) "lookup: Hay un identificador libre: x") 

(test (prueba '{{fun {x y} {+ x y}} {10 3}} ) (numV 13)) 

(test (prueba '{with* {{x 1} {y 2} {z 3}} {fun {x y x} {+ x {+ y z}}}})
      (closure '(x y x) (op + (list (id 'x) (op + (list (id 'y) (id 'z))))) (aSub 'z (num 3) (aSub 'y (num 2) (aSub 'x (num 1) (mtSub)))))) 

(test (prueba '{if0 {- 1 1} 5 6}) (numV 5)) 
(test (prueba '{if0 {+ 1 2} 5 6}) (numV 6)) 

(test/exn (prueba '{if0 {fun {x} {+ x 1}} 5 6}) "interp: Símbolo no esperado la condicional de if0, no es un número")

(test (prueba '{with {{f {fun {x} {+ x x}}}} {f {3}}}) (numV 6)) 

(test (prueba '{with {{x 3} {f {fun {a} {+ x a}}}}
                      {f {0}}}) "lookup: Hay un identificador libre: x")
