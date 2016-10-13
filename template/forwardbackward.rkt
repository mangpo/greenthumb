#lang racket

(require "../forwardbackward.rkt")

(provide $-forwardbackward%)

(define $-forwardbackward%
  (class forwardbackward%
    (super-new)
    (override len-limit)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) ?)

    ))
