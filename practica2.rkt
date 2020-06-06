#lang plai

;; 1.Función que recibe una lista y devuelve el conjunto potencia de los
;; elementos de dicha lista.
;; conjunto-potencia:(listof number) -> (listof (pairof number))

(define (cartesiano l1)
  (let ([p (car l1)]) (for/list ([x l1]) (list p x))))

(define (conjunto-cuadrado lista)
  (if (empty? lista)
      '()
      (append (cartesiano lista) (conjunto-cuadrado (cdr lista)))))

;; 2.Función que calcula el cambio que tenemos que devovler según el
;; monto a cobrar y el monto pagado. Devuelve la cantidad de monedas de las
;; denominaciones $50, $20, $10, $5, $2, $1.
;; area-cono: number number -> (number number number number number number)

(define (addcambio lista n)
  (cond [(zero? n) (cons (add1 (car lista)) (cdr lista))]
        [(equal? n 1) (map (lambda (n1 n2) (+ n1 n2)) lista (list 0 1 0 0 0))]
        [(equal? n 2) (map (lambda (n1 n2) (+ n1 n2)) lista '(0 0 1 0 0))]
        [(equal? n 3) (map (lambda (n1 n2) (+ n1 n2)) lista '(0 0 0 1 0))]
        [(equal? n 4) (map (lambda (n1 n2) (+ n1 n2)) lista '(0 0 0 0 1))]
        ))

(define (monedas dinero lista)
  (cond [(>= dinero 50) (monedas (- dinero 50) (addcambio lista 0))]
        [(and (>= 49 dinero) (>= dinero 20)) (monedas (- dinero 20) (addcambio lista 1))]
        [(and (>= 19 dinero) (>= dinero 10)) (monedas (- dinero 10) (addcambio lista 2))]
        [(and (>= 9 dinero) (>= dinero 5)) (monedas (- dinero 5) (addcambio lista 3))]
        [(and (>= 4 dinero) (>= dinero 1))  (monedas (- dinero 1) (addcambio lista 4))]
        [(zero? dinero) lista]))

(define (cambio total pago) (monedas (- total pago) (list 0 0 0 0 0)))


;; 4. Función que recibe n, r y devuelve el conjunto de múltiplos n,
;; en el rango n y r.
;; multiplos: number number -> (listof number)
;; usar listas x comprension y fun. anonimas
(define (multiplos n r)
  (map (lambda (x) (* x n))
       (for/list ([i (floor (/ r n))]) (+ i 1))))


;;5. Define una figura
(define-type Figura
  [Circulo (diametro number?)]
  [Cuadrado (lado number?)]
  [Rectángulo (base number?) (altura number?)]
  [Triángulo (base number?) (altura number?)])

;; 6.Función que recibe una figura y calcula su perímetro.
;; perimetro: Figura -> number
(define (perimetro figura)
  (match figura
    [(Circulo d) (* 2 pi (/ d 2))]
    [(Cuadrado l) (* l 4)]
    [(Rectángulo b a) (+ b a b a)]
    [(Triángulo b a) (* 3 b)]))

;; 7.Función que recibe una figura y calcula su área.
;; perimetro: Figura -> number
(define (area figura)
  (match figura
    [(Circulo d) (* pi (/ d 2) (/ d 2))]
    [(Cuadrado l) (* l l)]
    [(Rectángulo b a) (* b a)]
    [(Triángulo b a) (/ (* b a) 2)]))

;;8. Definición de arboles binarios de busqueda
(define-type ABB
  [vacio]
  [hoja (h number?)]
  [nodo (elem number?) (izq ABB?) (der ABB?)])

;; 9.Función que recibe un número, un árbol binario y agrega el elemento al
;; árbol de búsqueda binario.
;; agrega: number ABB -> number
;; los mayores o iguales van a la derecha
(define (agrega n arbol)
  (cond
    [(vacio? arbol) (hoja n)]
    [(hoja? arbol) (if (<= (hoja-h arbol) n)
                       (nodo (hoja-h arbol) (vacio) (hoja n))
                       (nodo (hoja-h arbol) (hoja n) (vacio)))]
    [(nodo? arbol) (if (<= (nodo-elem arbol) n)
                       (nodo (nodo-elem arbol) (nodo-izq arbol) (agrega n (nodo-der arbol)))
                       (nodo (nodo-elem arbol) (agrega n (nodo-izq arbol)) (nodo-der arbol)))]))

;; 10.Función que recibe un árbol binario y calcula su altura.
;; altura: ABB -> number
(define (altura arbol)
  (cond
    [(vacio? arbol) 0]
    [(hoja? arbol) 1]
    [(nodo? arbol)
     (+ 1 (max
           (altura (nodo-izq arbol))
           (altura (nodo-der arbol))))]
    ))

;; 11.Función que recibe un árbol binario, un elemento y
;; devuelve verdadero si el elemento está contenido en el árbol,
;; falso en otro caso.
;; contiene: ABB -> number -> boolean
(define (contiene arbol e)
  (cond
    [(vacio? arbol) #f]
    [(hoja? arbol) (if (equal? (hoja-h) e) #t #f)]
    [(nodo? arbol) (if (equal? (nodo-elem arbol) e)
                       #t
                       (and (contiene (nodo-izq arbol) e) (contiene (nodo-der arbol) e)))]))







