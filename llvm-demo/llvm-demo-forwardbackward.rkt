#lang racket

(require "../forwardbackward.rkt")

(provide llvm-demo-forwardbackward%)

(define llvm-demo-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer enum)
    (override len-limit extra-slots)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 5)
    (define (extra-slots) 3)

    ))
