#lang racket

(require "../stochastic.rkt" "$-machine.rkt")
(provide $-stochastic%)

(define $-stochastic%
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
      ? ;; modify this function

      ;; Example:

      ;; progstate-regs is a vector. We can use provided method correctness-cost-base
      ;; to compute correctness cost of a vector against another vector.
      ;; This method takes into account of misalignment.
      ;; For example, if r0 of state1 = r1 of state2, the cost will be quite low.
      (define cost-regs
        (correctness-cost-base (progstate-regs state1)
                               (progstate-regs state2)
                               (progstate-regs constraint)
                               diff-cost))

      ;; To calcuate correctness cost of memory object againt another,
      ;; simply call correctness-cost method of the memory object.
      (define cost-mem
        (if (progstate-memory constraint)
             (send (progstate-memory state1) correctness-cost
                   (progstate-memory state2) diff-cost bit)
             0))

      (+ cost-regs cost-mem))

    ))
           
