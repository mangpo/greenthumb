#lang s-exp rosette

(require "../symbolic.rkt")

(provide llvm-symbolic%)

(define llvm-symbolic%
  (class symbolic%
    (super-new)
    (override len-limit)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 3)
    ))
