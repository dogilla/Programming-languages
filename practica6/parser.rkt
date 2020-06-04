#lang plai
(require (file "./grammars.rkt"))

;; funciones que vuelven las operaciones booleanas una funcion multiparametros
(define (and . bools)
  (andmap (lambda (x) x) bools))

;; crea una binging a partir de una lista de 4 elementos y verifica la sintaxis
(define (create-binding l)
  (if (= (length l) 4)
      (if (and (symbol? (car l)) (equal? (second l) ':) (symbol? (third l)))
          (binding (car l) (tipo-de (third l)) (parse (fourth l)))
          (error "sintaxis incorrecta"))
      (error "Error sintactico en with")))

(define (tipo-de s)
  (match s
  ['number (numberT)]
  ['boolean (booleanT)]
  [else (numberT)]))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (match sexp
    [(? symbol?) (idS sexp)]
    [(? boolean?) (boolS sexp)]
    [(? number?) (numS sexp)]
    [(list 'with l body)
     (withS (map create-binding l) (parse body))]
    [(list 'with* l body)
     (withS (map create-binding l) (parse body))]
    [(list 'if con else then)
     (iFS (parse con) (parse else) (parse then))]))