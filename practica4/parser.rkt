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

(define (esOperador? expr)
  (match expr ['+ #t]['- #t]['* #t]['/ #t] ['expt #t] ['modulo #t]['add1 #t] ['sub1 #t] [else #f]))

(define (parse sexp)
  (match sexp
    ;;caso id
    [(? symbol?) (id sexp)]
    ;;caso numero
    [(? number?) (num sexp)]
    ;; caso with con desugar sintactica
    [(list 'with l body)
     (with (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    ;;caso if0
    [(list 'if0 cond then else)
     (if0 (parse cond) (parse then) (parse else))]
    [(list 'with* l body)
     (with* (map (lambda (x) (binding (car x) (parse (second x)) )) l) (parse body))]
    [(? list?) (op (opera (first sexp))  (map (lambda (x) (parse x)) (cdr sexp)))]
    [_ (error "Sintaxis Incorrecta")]))
