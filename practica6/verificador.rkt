#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; predicado que nos dice si una expresión es booleana, si es falso significa que
;; es numerica, error en otro caso.
(define (fun-booleana? expr)
  (match expr
    [+ #f][- #f][* #f][/ #f] [expt #f]
    [modulo #f] [add1 #f] [< #t] [<= #t] ['= equal?] ['not not]
    ['and and]
    ['or or]
    ['sub1 sub1] ['zero? zero?] [else (error "Operación fuera del lenguaje")]))

;; busca el tipo de un id
(define lookup-type
  (lambda (id
           context)
    (cond
      [(empty? context) 
       (error id "Indentificador sin tipo especificado: ")]
      
      [(symbol=? id (gamma-id (first context)))
       (first context)]
      
      [else (lookup-type id (rest context))])))

;; reviza el tipo de ambas condiciones de un if o cond
(define (type-conditions then else context)
  (let ([tipo (typeof then context)])
    (if (equal? tipo (typeof else context))
        tipo
        (error "Las condiciones de if deben ser del mismo tipo"))))

(define get-type-of-list
  (case-lambda
    [(l context)
     (if (empty? (cdr l))
         (typeof (car l) context)
         (get-type-of-list (cdr l) (typeof (car l) context) context))]
    [(l tipo context)
     (if (empty? l)
         tipo
         (if (equal? tipo (typeof (car l) context))
             (get-type-of-list (cdr l) tipo context)
             (error "Type Error")))]))


(define (op-numerica l)
  (if (empty? l)
      (numberT)
      (if (number? (car l))
          (op-numerica (cdr l))
          (error "Los argumentos de la operacion deben ser numeros"))))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof CFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
    [idS (i)
         (lookup-type expr context)]
    [boolS (b)
         (booleanT b)]
    [numS (n)
         (numberT n)]
    [iFS (cond else then)
         (if (booleanT? (typeof cond context))
             (type-conditions then else)
             (error "La condicion de if debe ser booleana"))]
    [opS (f args)
         ()]
    [condS (cases)
         ()]
    [withS (bindings body)
         ()]
    [withS* (bindings body)
          ()]
    [funS (p tipo body)
          ()]
    [appS (f args)
          ()]
    




    


    ))
  
(define (prueba exp)
  (typeof (parse exp) (phi)))