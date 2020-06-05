#lang plai
(require (file "./grammars.rkt"))

;; funciones que vuelven las operaciones booleanas una funcion multiparametros
(define (and . bools)
  (andmap (lambda (x) x) bools))

(define (or . bools)
  (andmap (lambda (x) x) bools))

(define expt
  (λ elem (foldr expt (car elem) (cdr elem))))

;; crea un binging a partir de una lista de 4 elementos y verifica la sintaxis
(define (create-binding l)
  (if (= (length l) 4)
      (if (and (symbol? (car l)) (equal? (second l) ':) (symbol? (third l)))
          (binding (car l) (tipo-de (third l)) (parse (fourth l)))
          (error "sintaxis incorrecta"))
      (error "Error sintactico en with")))

;; crea un param a partir de una lista de 2 elementos y verifica la sintaxis
(define (create-param l)
  (if (= (length l) 3)
      (if (and (symbol? (car l)) (equal? (second l) ':) (symbol? (third l)))
          (param (car l) (tipo-de (third l)))
          (error "sintaxis incorrecta"))
      (error "Error sintactico en fun")))

;; funcion que transforma el tipo de un symbol a Type
(define (tipo-de s)
  (match s
  ['number (numberT)]
  ['boolean (booleanT)]
  [else (error "Tipo no especificado en el lenguaje")]))

(define (opera expr)
  (match expr
    ['+ +]['- -]['* *]['/ /] ['expt (λ elem (foldr expt (car elem) (cdr elem)))]
    ['modulo modulo] ['add1 add1] ['< <] ['<= <=] ['= equal?] ['not not]
    ['and and]
    ['or or]
    ['sub1 sub1] ['zero? zero?] [else (error "Operación fuera del lenguaje")]))

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
     (iFS (parse con) (parse else) (parse then))]
    [(list 'fun l ': tipo body)
     (funS (map create-param l) (tipo-de tipo) (parse body))]
    [(list 'app f args)
     (appS (parse f) (map parse args))]
    [(cons x xs) (opS (opera x) (map (lambda (w) (parse w)) xs))]
    [? (error "Sintaxis Incorrecta")]))