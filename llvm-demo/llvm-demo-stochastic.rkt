#lang racket

(require "../stochastic.rkt")
(provide llvm-demo-stochastic%)

(define llvm-demo-stochastic%
  (class stochastic%
    (super-new)
    (inherit pop-count32 correctness-cost-base)
    (override correctness-cost extra-slots)
    
    (define (diff-cost x y)
      (pop-count32 (bitwise-xor (bitwise-and x #xffffffff) 
                                (bitwise-and y #xffffffff))))
    
    ;; Compute correctness cost sum of all bit difference in live variables.
    ;; state1: expected in progstate format
    ;; state2: actual in progstate format
    (define (correctness-cost state1 state2 constraint)
      (correctness-cost-base state1 state2 constraint diff-cost))

    (define (extra-slots) 4)

    ))
           
