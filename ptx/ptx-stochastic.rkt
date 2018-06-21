#lang racket

(require "../stochastic.rkt" "ptx-machine.rkt")
(provide ptx-stochastic%)

(define ptx-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine)
    (inherit pop-count32 pop-count64 correctness-cost-base)
    (override correctness-cost)

    (define bit (get-field bitwidth machine))

    ;; Count number of bits difference between x and y.
    (define (diff-cost x y)
      (pop-count32 (bitwise-xor (bitwise-and x #xffffffff) 
                                (bitwise-and y #xffffffff))))
    
    ;; Compute correctness cost.
    ;; state1: expected state
    ;; state2: actual state
    ;; constraint/live-out: program state that contains predicate
    ;;                      #t if the entry matters, #f otherwise.
    (define (correctness-cost state1 state2 constraint)
      ;; progstate-regs is a vector. We can use provided method correctness-cost-base
      ;; to compute correctness cost of a vector against another vector.
      ;; This method takes into account of misalignment.
      ;; For example, if r0 of state1 = r1 of state2, the cost will be quite low.
      (define cost-regs
        (correctness-cost-base (progstate-regs state1)
                               (progstate-regs state2)
                               (progstate-regs constraint)
                               diff-cost))
      
      (define cost-preds
        (correctness-cost-base (progstate-preds state1)
                               (progstate-preds state2)
                               (progstate-preds constraint)
                               diff-cost))

      (+ cost-regs cost-preds))

    ))
           

