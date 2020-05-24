#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; maneja el caso de iFS
(define (caseif lista)
  (let ([x (car lista)])
    (cond
    [(condition? x) (iF (desugar (condition-test-expr x))
                                  (desugar (condition-then-expr x))
                                  (caseif (cdr lista)))]
    [(else-cond? x) (desugar (else-cond-else-expr (car lista)))])))

;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
(define (desugar expr)
  (type-case SCFWBAE expr
    [idS (i) (id i)]
    [numS (n) (num n)]
    [boolS (b) (bool b)]
    [iFS (c t e) (if (desugar c) (desugar t) (desugar e))]
    [opS (p arg) (op p (map desugar arg))]
    [condS (l) (caseif l)]
    [appS (fun-expr args)
          (app (desugar fun-expr)
               (foldr (lambda (v l) (cons (desugar v) l)) '() args))]
    [funS (f body) (fun f (desugar body))]
    [withS (f body) (app (fun (map (lambda (v) (binding-id v)) f) (desugar body))
                         (map desugar (map (lambda (v) (binding-value v)) f)))]
    [withS* (f body) (cond
                       [(= 1 (length f)) (desugar(withS f body))]
                       [else (desugar (withS (list (car f)) (withS* (cdr f) body)))])]))
