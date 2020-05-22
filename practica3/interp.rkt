#lang plai
(require (file "./parser.rkt"))
(require (file "./grammars.rkt"))

;; Recibe una expresión (expr) del lenguaje WAE,
;; un id (sub-id) y otra expresión (value).
;; Sustituye el valor sub-id por value, en expr.
;; subst: WAE symbol WAE -> WAE
(define (subst expr sub-id value)
  (match expr
    [(num n) expr]
    [(id i) (if (equal? sub-id i)
            value
            expr)]
    ;; le aplica susbt a cada elemento de la operacion
    [(op f args) (op f  (map (lambda (elemento) (subst elemento sub-id value)) args))]
    ;;
    [(with l c) (with (map (lambda (x) (binding (binding-id x) (subst (binding-value x) sub-id value))) l)       
                      (if (symbol=? (binding-id (car l)) sub-id)
                          c
                          (subst c sub-id value)))]
    ;; lo mismo con with* porque la ligadura es parecida
    [(with* l c) (with* (map (lambda (x) (binding (binding-id x) (subst (binding-value x) sub-id value))) l)       
                      (if (symbol=? (binding-id (car l)) sub-id)
                          c
                          (subst c sub-id value)))]))


;; Toma un árbol de sintáxis abstraca del lenguaje WAE
;; y lo interpreta, devolviendo el valor numérico correspondiente
;; interp: WAE -> number
(define (interp expr)
  (match expr
    [(num n) n]
    [(id i) (error "error: Variable libre")]
    [(op f l) (if (empty? (cddr l))
                  (f (interp (car l)) (interp (cadr l)))
                  (interp (op f (cons (num (f (interp (car l)) (interp (cadr l)))) (cddr l)))))]

    [(with l b)
     (interp (foldr (lambda (l e)
                      (subst e (binding-id l) (num (interp (binding-value l))))) b l))]
    [(with* l b)
     (interp (with (interp l) b))]
    ;; si llegamos a este caso se trata de una lista de binging de with* o with
    [(? list) (if (empty? (cdr expr))
                  expr
                  (cons (car expr) (interp (map (lambda (v)
                                                  (binding (binding-id v)
                                                           (subst (binding-value v)
                                                                (binding-id (car expr))
                                                                (binding-value (car expr))))) (cdr expr)))))]))