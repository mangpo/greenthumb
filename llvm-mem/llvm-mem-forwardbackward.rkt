#lang racket

(require "../forwardbackward.rkt")

(provide llvm-mem-forwardbackward%)

(define llvm-mem-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer enum)
    (override len-limit)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 5)

    ))
