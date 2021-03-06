#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE
;; (define (lookup name ds)
(define (lookup sub-name env)
  (match env
    [(mtSub) (error "lookup: Hay un identificador libre")]
    [(aSub name value rest-env)
     (if (symbol=? name sub-name)
         value
         (lookup sub-name rest-env))]))

;;evalua una condicion
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

(define (eval-operation f lista ds)
  (if (number? (car (saca-valor lista ds)))
      (numV (foldr f 0 (saca-valor lista ds)))
      (boolV (apply f (saca-valor lista ds)))))


(define (crea-ambiente params args env)
  (match params
    ['() env]
    [(cons x xs)
     (cond
       [(empty? args) (error "Faltan argumentos")]
       [else (crea-ambiente xs (cdr args) (aSub x (car args) env))])]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
(define (interp expr ds)
  (type-case CFWBAE expr
    [id (i)
        (lookup i ds)]
    [num (n)
         (numV n)]
    [bool (b)
          (boolV b)]
    [iF (c then else)
        (if (eval-cond c ds) (interp then ds) (interp else ds))]
    [op (f a)
        (eval-operation f a ds)]
    [fun (lista body)
         (closure lista body ds) ]
    [app (fun-expr argumentos)
         (let ([fun-val (interp fun-expr ds)])
           (interp (closure-body fun-val)
                   (crea-ambiente (closure-param fun-val) argumentos ds)))]))


