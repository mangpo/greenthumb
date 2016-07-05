#lang s-exp rosette

(require "../validator.rkt")
(provide llvm-mem-validator%)

(define llvm-mem-validator%
  (class validator%
    (super-new)
    (override get-constructor)

    (define (get-constructor) llvm-mem-validator%)

    ))
