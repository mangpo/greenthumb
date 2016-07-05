#lang s-exp rosette

(require "../validator.rkt")
(provide llvm-demo-validator%)

(define llvm-demo-validator%
  (class validator%
    (super-new)
    (override get-constructor)

    (define (get-constructor) llvm-demo-validator%)

    ))
