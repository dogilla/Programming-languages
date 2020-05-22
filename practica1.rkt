#lang racket

;;Practica 1 - Lenguajes de Programacion 2020-2



;; 1.Funcion que calcula el area de un cono de base circular
;; area-cono: number number -> number

(define (area-cono d g)
  (let ([radio (/ d 2)])
  (+ (* pi radio g) (* pi radio radio))))

;; 2. Funcion que eleva el numero a, a la potencia b
;; potencia: number -> number

;; b es positivo
(define (potencia-positiva a b)
  (if (zero? b)
      1
      (* a (potencia-positiva a (- b 1)))))

;; b es negativo
(define (potencia-neg a b)
  (/ 1 (potencia-positiva a (* -1 b))))
  
(define (potencia a b)
  (cond [(and (zero? a) (zero? b)) (error "no definido")]
        [(< b 0) (potencia-neg a b)]
        [(>= b 0) (potencia-positiva a b)]))


;; 3.Funcion que calcula la distancia entre dos puntos en el plano
;; distancia: (pairof number) (pairof number) -> number
(define (distancia p q)
  (sqrt (+ (expt (- (car q)(car p)) 2) (expt (- (cadr q)(cadr p)) 2))))

;; 4.Predicado que nos dice si un numero es negativo
;; neg?: number -> Boolean
(define (neg? a)
  (if (< a 0) #t #f))

;; 5. Funcion que nos devuelve el valor absoluto de un numero
;; absoluto: number -> number
(define (absoluto n)
  (if (neg? n) (* -1 n) n))

;; 6. Predicado que nos dice si un numero m es divisor de otro numero n
;; divisor?: number number -> number
(define (divisor? m n)
  (if (zero? (modulo n m)) #t #f))

;; 7.Funcion que nos da la longitud de una lista
;; longitud: (listof a) -> number
(define (longitud lista)
  (if (equal? lista '()) 0 (+ 1 (longitud (cdr lista)))))

;; 8. Funcion que nos da el elemento maximo de una lista
;; maximo: (listof a) -> number

;; funcion auxiliar para sacar el maximo
;; h es la cabeza (head) y r es el resto (rest)

(define (aux_max h r)
  (cond
    [(equal? r '()) h]
    [(>= (car r) h) (aux_max (car r) (cdr r))]
    [(< (car r) h) (aux_max h (cdr r))]))

(define (maximo lista)
  (if (equal? (longitud lista) 1) (car lista) (aux_max (car lista) (cdr lista))))

;; 9.Funcion que nos da una lista invertida de la lista pasada como parametro
;; reversa-lista: (listof a) -> (listof a)

(define (aux-reversa accumulador lista)
  (if (empty? lista)
       accumulador
      (aux-reversa (cons (first lista) accumulador) (rest lista))))

(define (reversa-lista lista)
  (if (empty? lista) '() (aux-reversa '() lista)))

;; 10.Predicado que nos dice si una lista contiene elementos que forman un palindromo
;; palindromo-lista?: (listof a) -> Boolean

(define (palindromo? lista)
  (if (empty? lista) #t (equal? (reversa-lista lista) lista)))

;; 11.Funcion que nos da el una lista de divisores de un numero pasado como parametro
;; divisores: number -> (listof number).

(define (divisores-acc contador numero lista)
  (if (equal? contador numero)
      (cons numero lista)
      ;; else
      (if (zero? (modulo numero contador))
          (divisores-acc (+ contador 1) numero (cons contador lista))
          (divisores-acc (+ contador 1) numero lista))))

(define (divisores n) (divisores-acc 1 n '()))