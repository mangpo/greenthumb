#lang racket

(require "../stochastic.rkt" "llvm-machine.rkt")
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

    (define (correctness-cost-vector n s1 s2 c)
      (define cost 0)
      (for ([i n])
           (set! cost (+ cost
                         (correctness-cost-base (vector-map (lambda (x) (vector-ref x i)) s1)
                                                (vector-map (lambda (x) (vector-ref x i)) s2)
                                                c diff-cost))))
      cost)
    
    ;; Compute correctness cost sum of all bit difference in live variables.
    ;; state1: expected in progstate format
    ;; state2: actual in progstate format
    (define (correctness-cost state1 state2 constraint)
      (+ (correctness-cost-base (progstate-var state1)
                                (progstate-var state2)
                                (progstate-var constraint)
                                diff-cost)
         (correctness-cost-vector 4 (progstate-vec4 state1)
                                  (progstate-vec4 state2)
                                  (progstate-vec4 constraint))
         (if (vector-ref constraint 1)
             (send (progstate-memory state1) correctness-cost
                   (progstate-memory state2) diff-cost bit)
             0)))

    ))
           
