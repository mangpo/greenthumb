#lang racket

(require "../meta.rkt")

(provide neon-meta%)

(define neon-meta%
  (class meta%
    (super-new)
    (override required-module get-class-name)

    (define (required-module x) (format "neon/neon-~a.rkt" x))
    (define (get-class-name x) (format "neon-~a%" x))
    
    ))