#lang racket

(require "../stochastic.rkt")
(provide $-stochastic%)

(define $-stochastic%
  (class stochastic%
    (super-new)
    (inherit pop-count32 correctness-cost-base)
    (override correctness-cost)
    
    ;; Compute correctness cost.
    ;; state1: expected state
    ;; state2: actual state
    ;; constraint/live-out: program state that contains predicate
    ;;                      #t if the entry matters, #f otherwise.
    (define (correctness-cost state1 state2 constraint) ?)

    ))
           
