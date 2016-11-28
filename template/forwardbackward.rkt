#lang racket

(require "../forwardbackward.rkt")

(provide $-forwardbackward%)

(define $-forwardbackward%
  (class forwardbackward%
    (super-new)
    (override len-limit)

    ;; Number of instructions that can be synthesized within a minute.
    ;; Try setting it to 5 to start and adjust it later.
    (define (len-limit) 5)

    ))
