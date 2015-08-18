#lang racket

(provide enumerative%)

(define enumerative%
  (class object%
    (super-new)
    ;; See arm/arm-enumerative.rkt
    (abstract get-flag generate-inst)
    ))