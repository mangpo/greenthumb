#lang racket

(provide enumerative%)

(define enumerative%
  (class object%
    (super-new)
    ;; See arm/arm-enumerative.rkt
    (abstract generate-inst)
    (public get-flag)

    (define (get-flag state) #f)
    ))
