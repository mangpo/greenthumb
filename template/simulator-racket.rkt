#lang racket

(require "../simulator-racket.rkt" "../ops-racket.rkt" "../inst.rkt")
(provide $-simulator-racket%)

(define $-simulator-racket%
  (class simulator-racket%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) $-simulator-racket%)

    (define bit (get-field bit machine))
    (define nop-id (get-field nop-id machine))
    (define inst-id (get-field inst-id machine))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Truncate x to 'bit' bits and convert to signed number.
    ;; Always use this macro when interpreting an operator.
    (define-syntax-rule (finitize-bit x) (finitize x bit))
    (define-syntax-rule (bvop op)     
      (lambda (x y) (finitize-bit (op x y))))
    (define (shl a b) (<< a b bit))
    (define (ushr a b) (>>> a b bit))

    ;; Binary operation.
    (define bvadd  (bvop +))
    (define bvsub  (bvop -))
    (define bvshl  (bvop shl))
    (define bvshr  (bvop >>))   ;; signed shift right
    (define bvushr (bvop ushr)) ;; unsigned shift right
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Required methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Interpret a given program from a given state.
    ;; Policy argument is always #f unless for advanced usage.
    (define (interpret program state [policy #f]) ?)

    ;; Estimate performance cost of a given program.
    (define (performance-cost program) ?)

    ))
