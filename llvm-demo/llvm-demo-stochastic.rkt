#lang racket

(require "../stochastic.rkt"
         "../inst.rkt" 
         "../machine.rkt" "llvm-demo-machine.rkt")
(provide llvm-demo-stochastic%)

(define llvm-demo-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine stat mutate-dist live-in)
    (inherit random-args-from-op mutate pop-count32 correctness-cost-base)
    (override correctness-cost)
    
    (define (diff-cost x y)
      (pop-count32 (bitwise-xor (bitwise-and x #xffffffff) 
                                (bitwise-and y #xffffffff))))
    
    ;; Compute correctness cost sum of all bit difference in live variables.
    ;; state1: expected in progstate format
    ;; state2: actual in progstate format
    (define (correctness-cost state1 state2 constraint)
      (correctness-cost-base state1 state2 constraint diff-cost))

    ))
           
