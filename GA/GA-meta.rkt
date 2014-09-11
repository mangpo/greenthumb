#lang racket

(require "../meta.rkt")

(provide GA-meta%)

(define GA-meta%
  (class meta%
    (super-new)
    (override required-module get-class-name)

    (define (required-module x) (format "GA/GA-~a.rkt" x))
    (define (get-class-name x) (format "GA-~a%" x))
    
    ))
