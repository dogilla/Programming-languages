#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; predicado que nos dice si el tipo del que deberia ser un procedimiento
;; en una operación

(define (type-fun fun)
  (cond
    [(equal? fun and) (booleanT)]
    [(equal? fun or) (booleanT)]
    [(equal? fun not) (booleanT)]
    [else (numberT)]))

;; busca el tipo de un id en el contexto
(define lookup-type
  (lambda (id
           context)
    (cond
      [(phi? context) 
       (error  "Identificador sin tipo especificado: " (idS-i id))]
      
      [(symbol=? (idS-i id) (gamma-id context))
       (gamma-tipo context)]
      
      [else (lookup-type id (gamma-rest context))])))

;; crea un contexto a partir de una lista de ligaduras
(define (context-from-bindigs b context)
  (if (empty? b)
      context
      (context-from-bindigs (cdr b)
                            (expand-context context
                                            (binding-id (car b))
                                            (binding-tipo (car b))))))

;;expande un contexto
(define (expand-context context id tipo)
  (if (phi? context)
      (gamma id tipo (phi))
      (gamma id tipo context)))

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

;; crea una lista de tipos a partir de los parametros de una funcion
(define (create-list-type params)
   (map (lambda (x) (param-tipo x)) params))

(define (check-params p context)
  (if (empty? p)
      #t
      (if (equal? (binding-tipo (car p)) (typeof (binding-value (car p)) context))
          (check-params (cdr p) context)
          (error "los parametros de un with o with* deben ser del mismo al declarado"))))

;; crea un contexto a partir de la lista de parametros de una funcion
(define (create-context-from-params params context)
  (if (empty? params)
      context
      (create-context-from-params (cdr params)
                            (expand-context context
                                            (param-param (car params))
                                            (param-tipo (car params))))))


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
    [funS (p tipo body)
          (if (equal? tipo (typeof body (create-context-from-params p context)))
              (funT (append (create-list-type p) (list tipo)))
              (error "el cuerpo de la funcion no corresponde con su tipo esperado"))]
    [withS (bindings body)
           (if (check-params bindings context)
               (typeof body (context-from-bindigs bindings context))
               (error "los parametros de un with o with* deben ser del mismo al declarado")) ]
    [withS* (bindings body)
            (if (check-params bindings context)
               (typeof body (context-from-bindigs bindings context))
               (error "los parametros de un with o with* deben ser del mismo al declarado"))]
    [appS (f args)
          (if (funT? (typeof f context))
              (if (equal? (create-list-type (funS-params f)) (map (lambda (x) (typeof x context)) args))
                  (typeof f context)
                  (error "los argumentos de la aplicación no coinciden con los declarados en la función"))
              ;;(typeof args (create-context-from-params (funS-params f) context))
              (error "La expresión izquierda de una aplicación debe ser una función"))]))
  
(define (prueba exp)
  (typeof (parse exp) (phi)))
