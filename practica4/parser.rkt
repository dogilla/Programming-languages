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

;;funcion que elimina las expresiones with semanticamente y las vuelve azucar sintactica
;; FWAE -> FWAE
(define (multi-with ligaduras cuerpo)
  (if (= 1 (length ligaduras))
      (with ligaduras (parse cuerpo))
      (multi-with (with (list (car ligaduras)) (with* (cdr ligaduras) cuerpo)))))

;; funcion que saca identificadores
(define (get-id exp)
  (match exp
    [(app* id body) id]
    [(binding id valor) (binding-id exp)]))

(define (parse sexp)
  (match sexp
    ;;caso id
    [(? symbol?) (id sexp)]
    ;;caso numero
    [(? number?) (num sexp)]
    ;; caso with quitando el azucar sintactica
    [(list 'with l body)
     (app (fun (map (lambda (x) (get-id (parse x)) ) l) (parse body))
          (map (lambda (x) (parse (second x))) l) )]
    ;;caso if0
    [(list 'if0 cond then else)
     (if0 (parse cond) (parse then) (parse else))]
    [(list 'with* l body)
     (parse (multi-with sexp))]
    ;;caso lista
    [(cons x xs)
     (cond
       [(esOperador? x) (op (opera (first sexp))  (map (lambda (x) (parse x)) (cdr sexp)))]
       [(number? x) (cons (parse x) (parse xs))]
       [else (app* x (if (CFWAE? (parse (car xs))) (list (parse (car xs))) (parse (car xs))))]

       ;;  (map (lambda (x) (parse x))
       )]

     
    [_ sexp]))
