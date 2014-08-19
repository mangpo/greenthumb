#lang racket

(require "ast.rkt")
(provide simulator%)

(define simulator%
  (class object%
    (super-new)
    (abstract interpret performance-cost)
    ))