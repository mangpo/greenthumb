#lang racket

(require "../stochastic.rkt")
(provide llvm-stochastic%)

(define llvm-stochastic%
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
      (+ (correctness-cost-base (progstate-var state1)
                                (progstate-var state2)
                                (progstate-var constraint)
                                diff-cost)
         (if (vector-ref constraint 1)
             (send (progstate-memory state1) correctness-cost
                   (progstate-memory state2) diff-cost bit)
             0)))

    ))
           
