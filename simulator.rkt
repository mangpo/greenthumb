#lang racket

(provide simulator%)

(define simulator%
  (class object%
    (super-new)
    (abstract interpret performance-cost)
    (public is-valid? get-constructor)
    
    (define (get-constructor) (raise "Please implement simulator:get-constructor"))
    (define (is-valid? code) #t)
    ))
