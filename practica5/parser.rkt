#lang plai
(require (file "./grammars.rkt"))

(define (simbolo s)
  (cond
    [(equal? s 'true) (boolS #t)]
    [(equal? s 'false) (boolS #f)]
    [else (idS s)]))


(define (saca par)
  (if (equal? (car par) 'else)
      (else-cond (parse (second par)))
      (condition (parse (first par)) (parse (second par)))))

(define (opera expr)
  (match expr ['+ +]['- -]['* *]['/ /] ['expt (λ elem (foldr expt (car elem) (cdr elem)))]
              ['modulo modulo] ['add1 add1] ['< <] ['<= <=] ['= equal?] ['not not]
              ['and (and)] ['or (or)] ['sub1 sub1] ['zero? zero?] [else (error "Operación fuera del lenguaje")]))

;;funcion que nos dice si es opedador de numeros
(define (esOperadorn? expr)
  (match expr ['+ #t]['- #t]['* #t]['/ #t] ['expt #t] ['modulo #t]['add1 #t] ['sub1 #t] [else #f]))

;; funcion que nos dice si es operado de booleanos
(define (esOperadorb? expr)
  (match expr
    ['modulo #t] ['add1 #t] ['< #t] ['<= #t] ['= #t] ['not #t] ['and #t] ['or #t] ['sub1 #t] ['zero? #t] [else #f]))


;;     [app* (id argumentos) (interp (app (lookup id ds) argumentos) ds)]

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (match sexp
    [(? symbol?)
     (simbolo sexp)]
    [(? boolean?) (boolS sexp)]
    [(? number?)
     (numS sexp)]
    [(list 'with l body)
     (withS (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    [(list 'with* l body)
     (withS* (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    [(list 'if con else then)
     (iFS (parse con) (parse else) (parse then))]
    [(list 'fun l body)
     (funS l (parse body))]
    [(list 'cond casos)
     (condS (foldr (lambda (v l) (cons (saca v) l)) '() casos))]
    [(list 'app f args)
     (appS (parse f) (map parse args))]
    [(cons x xs)
     (cond  [(or (esOperadorb? x) (esOperadorn? x)) (opS (opera x) (map (lambda (w) (parse w)) xs))])
     ]))