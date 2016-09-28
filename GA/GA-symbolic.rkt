#lang s-exp rosette

(require "../symbolic.rkt" "../inst.rkt")

(provide GA-symbolic%)

(define GA-symbolic%
  (class symbolic%
    (super-new)
    (inherit sym-op sym-arg)
    (override len-limit window-size gen-sym-inst)

    (define (len-limit) 8)
    (define (window-size) 14)
    
    (define (gen-sym-inst)  (inst (sym-op) (vector (sym-arg))))

    ))
