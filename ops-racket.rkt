#lang racket

(require "neon/machine.rkt")
(provide (all-defined-out))

(define-syntax-rule (<< x y)
  (if (and (>= y 0) (< y bit))
      (let ([mask (sub1 (arithmetic-shift 1 (- bit y)))])
        (arithmetic-shift (bitwise-and x mask) y))
      0))

(define-syntax-rule (>> x y)
  (if (>= y 0)
      (arithmetic-shift x (- y))
      (if (>= x 0) 0 -1)))

(define-syntax-rule (>>> x y)
  (if (= y 0)
      x
      (let ([unsigned-x (bitwise-and x (sub1 (arithmetic-shift 1 bit)))])
        (>> unsigned-x y))))

(define (finitize num [bit bit]) 
  (let* ([mask (arithmetic-shift -1 bit)]
         [masked (bitwise-and (bitwise-not mask) num)])
    (if (bitwise-bit-set? masked (- bit 1))
        (bitwise-ior mask masked)  
        masked)))

(define-syntax assert
  (syntax-rules ()
    ((assert x) 
     (unless x (raise "assert fail")))
    ((assert x y) 
     (unless x (raise (format "assert fail :~a" y))))))