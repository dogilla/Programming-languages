#lang plai
(require (file "./grammars.rkt"))

(define (simbolo s)
  (cond
    [(equal? s 'true) (bool #t)]
    [(equal? s 'false) (bool #f)]
    [else (idS s)]))


(define (saca par)
  (if (equal? (car par) 'else)
      (else-cond (parse (second par)))
      (condition (parse (first par)) (parse (second par)))))

(define (opera expr)
  (match expr ['+ +]['- -]['* *]['/ /] ['expt (λ elem (foldr expt (car elem) (cdr elem)))]
                                        ['modulo modulo]['add1 add1] ['sub1 sub1] [else (error "NO")]))
(define (esOperador? expr)
  (match expr ['+ #t]['- #t]['* #t]['/ #t] ['expt #t] ['modulo #t]['add1 #t] ['sub1 #t] [else #f]))

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
     [(esOperador? x) (op (opera (first sexp))  (map (lambda (x) (parse x)) (cdr sexp)))]]))