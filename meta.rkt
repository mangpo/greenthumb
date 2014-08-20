#lang racket

(provide meta%)

(define meta%
  (class object%
    (super-new)
    (abstract required-module get-class-name)
    ))