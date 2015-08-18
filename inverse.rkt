#lang racket

(provide inverse%)

(define inverse%
  (class object%
    (super-new)
    (abstract gen-inverse-behavior interpret-inst)
    ))