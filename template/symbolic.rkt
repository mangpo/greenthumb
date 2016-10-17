#lang s-exp rosette

(require "../symbolic.rkt")

(provide $-symbolic%)

(define $-symbolic%
  (class symbolic%
    (super-new)
    (override len-limit)

    ;; Num of instructions that can be synthesized within a minute.
    ;; Try setting it to 3 to start and adjust it later.
    (define (len-limit) 3)
    
    ))
