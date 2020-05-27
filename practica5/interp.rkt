#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE
;; (define (lookup name ds)
(define (lookup name ds)
  (match ds
    [(mtSub) (error "lookup: Hay un identificador libre")]
    [(aSub n value ds) (if (equal? name n)
                           value
                           (lookup name ds))]))


(define (eval-cond c ds)
  (let ([result (interp c ds)])
    (cond
      [(bool? c) (boolV-b result)]
      [else (error "La condicion no es booleana")])))

(define (interp-lista lista ds)
  (map (lambda (x) (if (CFWBAE? x) x (interp x ds))) lista))

;; funcion que evalua la lista de valores de una operacion
(define (saca-valor lista ambiente)
  (let ([value (interp (car lista) ambiente)] [list-of-value (map (lambda (x) (interp x ambiente)) lista)])
    (cond
      [(numV? value) (map (lambda (z) (if (numV? z)
                                          (numV-n z)
                                          (error "No se puede operar booleanos con numeros"))) list-of-value)]
      [(boolV? value)(map (lambda (z) (if (boolV? z)
                                          (boolV-b z)
                                          (error "No se puede operar booleanos con numeros"))) list-of-value)]
      ;;interpreta los valores hasta obtener un numV
      [else (saca-valor (interp-lista list-of-value ambiente) ambiente)])))

(define (create-env params args env)
  (match params
    ['() env]
    [(cons x xs)
     (cond
       [(empty? args) error 'create-env "Missing arguments"]
       [else (create-env xs (cdr args) (aSub x (car args) env))])]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)
  (type-case CFWBAE expr
    [id (i)
        (lookup expr ds)]
    [num (n)
         (numV n)]
    [bool (b)
          (boolV b)]
    [iF (c then else)
        (if (eval-cond c ds) (interp then ds) (interp else ds))]
    [op (f a)
        (let ([lista (saca-valor a ds)])
          (if (numV? (car lista))
              (numV (foldr f 0 lista))
              (boolV (foldr f lista))))]
       
    [fun (lista body)
         (closure lista body ds) ]
    [app (fun-expr argumentos)
         (let ([fun-val (interp fun-expr ds)])
           (interp (closure-body fun-val)
                   (create-env (closure-param fun-val) argumentos ds)))]))