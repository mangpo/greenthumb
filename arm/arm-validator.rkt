#lang s-exp rosette

(require "../validator.rkt")
(provide arm-validator%)

(define arm-validator%
  (class validator%
    (super-new)
    (override get-constructor)
    (define (get-constructor) arm-validator%)

    ))
