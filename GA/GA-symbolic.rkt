#lang s-exp rosette

(require "../symbolic.rkt" "../inst.rkt")

(provide GA-symbolic%)

(define GA-symbolic%
  (class symbolic%
    (super-new)
    (override len-limit window-size)

    (define (len-limit) 8)
    (define (window-size) 14)

    ))
