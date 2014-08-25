#lang racket

(require "ast.rkt")
(provide simulator%)

(define simulator%
  (class object%
    (super-new)
    (abstract interpret performance-cost)
    (public is-valid?)

    (define (is-valid? code) #t)
    ))