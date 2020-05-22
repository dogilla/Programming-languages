#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> CFWAE
;; parse: s-expression -> CFWAE

(define (opera expr)
  (match expr ['+ +]['- -]['* *]['/ /] ['expt (λ elem (foldr expt (car elem) (cdr elem)))]
                                        ['modulo modulo]['add1 add1] ['sub1 sub1] [else (error "NO")]))

(define (parse sexp)
  (match sexp
    [(? symbol?) (id sexp)]
    [(? number?) (num sexp)]
    [(list 'with l body)
     (with (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    [(list 'with* l body)
     (with* (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    [(? list?) (op (opera (first sexp))  (map (lambda (x) (parse x)) (cdr sexp)))]
    [_ (error "Sintaxis Incorrecta")]))
