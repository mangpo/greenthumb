#lang s-exp rosette
(require "../validator.rkt")
(provide ptx-validator%)

(define ptx-validator%
  (class validator%
    (super-new)
    (override get-constructor)

    (define (get-constructor) ptx-validator%)

    ))

