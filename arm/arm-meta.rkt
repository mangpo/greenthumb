#lang racket

(require "../meta.rkt")

(provide arm-meta%)

(define arm-meta%
  (class meta%
    (super-new)
    (override required-module get-class-name)

    (define (required-module x) (format "arm/arm-~a.rkt" x))
    (define (get-class-name x) (format "arm-~a%" x))
    
    ))