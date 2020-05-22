#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWAE
;; (define (lookup name ds)
(define (lookup name ds)
  (match ds
    [(mtSub) (error "lookup: Hay un identificador libre: x")]
    [(aSub n value ds) (if (equal? name n)
                           value
                           (lookup name ds))]))

;; funcion que crea una lista de valores a los que se les va a aplicar un
;; procedimiento

(define (interp-lista lista ds)
  (map (lambda (x) (if (CFWAE? x) x (interp x ds))) lista))

(define (saca-valor lista ambiente)
  (let ([value (interp (car lista) ambiente)] [list-of-value (map (lambda (x) (interp x ambiente)) lista)])
    (cond
      [(numV? value) (map numV-n list-of-value)]
      ;;interpreta los valores hasta obtener un numV
      [else (saca-valor (interp-lista list-of-value ambiente) ambiente)])))

;; evalua la condicion del if0
(define (eval-cond cond ds)
  (let ([valor (interp cond ds)])
    (if (zero? (numV-n valor))
        #t
        #f)))

(define (create-env params args env)
  (match params
    ['() env]
    [(cons x xs)
     (cond
       [(empty? args) error 'create-env "Missing arguments"]
       [else (create-env xs (cdr args) (aSub x (car args) env))

             ])]))

;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWAE DefrdSub-> number
(define (interp expr ds)
  (type-case CFWAE expr
    [id (i)
        (lookup i ds)]
    [num (n)
         (numV n)]
    [op (f args)
        (numV (foldr f 0 (saca-valor args ds)))]
    [if0 (cond then else)
         (if (eval-cond cond ds) (interp then ds) (interp else ds))]
    [fun (lista body)
         (closure lista body ds)]
    [app (fun-expr argumentos)
         (let ([fun-val (interp fun-expr ds)])
           (interp (closure-body fun-val)
                   (create-env (closure-param fun-val) argumentos ds)))]
    [app* (id argumentos)
          (interp (app (lookup id ds) argumentos) ds)]
    ;;en teoria no deberiamos llegar aqui
    [with* (f body)
          (expr)]
    [with (ligaduras body)
          (expr)]))