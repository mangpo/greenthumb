#lang racket

(require "GA-machine.rkt")
(provide (all-defined-out))

(define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
(define-syntax-rule (modulo+ x y) (if (>= x 8) (- x y) x))

;; Get item i from the stack
(define (get-stack stack i)
  (vector-ref (stack-body stack) (modulo- (- (stack-sp stack) i) 8)))
