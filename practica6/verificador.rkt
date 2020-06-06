#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; predicado que nos dice si el tipo del que deberia ser un procedimiento
;; en una operación

(define (type-fun fun)
  (cond
    [(equal? fun +) (numberT)]
    [(equal? fun -) (numberT)]
    [(equal? fun *) (numberT)]
    [(equal? fun /) (numberT)]
    [(equal? fun sub1) (numberT)]
    [(equal? fun zero?) (numberT)]
    [(equal? fun expt) (numberT)]
    [(equal? fun modulo) (numberT)]
    [(equal? fun add1) (numberT)]
    [(equal? fun <) (numberT)]
    [(equal? fun <=) (numberT)]
    [(equal? fun equal?) (numberT)]
    [(equal? fun and) (booleanT)]
    [(equal? fun or) (booleanT)]
    [(equal? fun not) (booleanT)]))

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

;; regresa una lista con los tipos de las condicionales de un CondS
(define (type-cond c)
  (cond
    [(condition? c) (if (booleanT? (typeof (condition-test-expr c)))
                        (typeof (condition-then-expr c))
                        (error "Las condiciones de cond deber ser booleanas"))]
    [(else-cond? c) (typeof (else-cond-else-expr c))]))

;; listof(type) -> type
(define (principal-cond-type lst)
  (if (= (length lst) 1)
      (car lst)
      (if (equal? (car lst) (principal-cond-type (cdr lst)))
          (car lst)
          (error "Las expresiones de una condicion deben tener el mismo tipo"))))

;; reviza que todos los elementos de una lista tengan el mismo tipo y
;; lo devuelve si en efecto son iguales
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
             (error "Type Error: Argumentos de difente tipo")))]))


;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof CFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
    [idS (i)
         (lookup-type expr context)]
    [boolS (b)
         (booleanT)]
    [numS (n)
         (numberT)]
    [iFS (cond else then)
         (if (booleanT? (typeof cond context))
             (type-conditions then else)
             (error "La condicion de if debe ser booleana"))]
    [opS (f args)
         (let [(tipo (get-type-of-list args context))]
           (if (equal? (type-fun f) tipo)
               tipo
               (error "Type Error: El procedimiento de la funcion no coincide con el sus argumentos")))]
    [condS (cases)
           (principal-cond-type (map type-cond cases))]
    [withS (bindings body)
         body]
    [withS* (bindings body)
          body]
    [funS (p tipo body)
          body]
    [appS (f args)
          args]))
  
(define (prueba exp)
  (typeof (parse exp) (phi)))
