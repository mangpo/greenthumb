#lang s-exp rosette

(require "../validator.rkt")
(provide llvm-validator%)

(define llvm-validator%
  (class validator%
    (super-new)
    (override get-constructor)

    (define (get-constructor) llvm-validator%)

    ))
