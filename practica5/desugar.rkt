#lang plai
(require (file "./grammars.rkt"))
;;(require (file "./parser.rkt"))


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
    [appS (fun-expr args) (app (desugar fun-expr) (foldr (λ (v l) (cons (desugar v) l)) '() args))]
    [funS (f body) (fun f (desugar body))]
    [withS (f body) (app (fun (map (λ (v) (binding-name v)) f) (desugar body)) (map desugar (map (λ (v) (binding-value v)) f)))]
    [withS* (f body) (cond
                       [(= 1 (length f)) (desugar(withS f body))]
                       [else (desugar (withS (list (car f)) (withS* (cdr f) body)))])]))
