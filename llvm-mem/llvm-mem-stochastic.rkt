#lang racket

(require "../stochastic.rkt")
(provide llvm-mem-stochastic%)

(define llvm-mem-stochastic%
  (class stochastic%
    (super-new)
    (inherit pop-count32 correctness-cost-base)
    (inherit-field machine)
    (override correctness-cost)

    (define bit (get-field bitwidth machine))
    
    (define (diff-cost x y)
      (pop-count32 (bitwise-xor (bitwise-and x #xffffffff) 
                                (bitwise-and y #xffffffff))))
    
    ;; Compute correctness cost sum of all bit difference in live variables.
    ;; state1: expected in progstate format
    ;; state2: actual in progstate format
    (define (correctness-cost state1 state2 constraint)
      (+ (correctness-cost-base (vector-ref state1 0)
                                (vector-ref state2 0)
                                (vector-ref constraint 0)
                                diff-cost)
         (if (vector-ref constraint 1)
             (send (vector-ref state1 1) correctness-cost
                   (vector-ref state2 1) diff-cost bit)
             0)))

    ))
           
