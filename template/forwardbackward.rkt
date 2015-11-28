#lang racket

(require "../forwardbackward.rkt" "../inst.rkt")

(provide $-forwardbackward%)

(define $-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine printer)
    (override len-limit change-inst change-inst-list)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) ?)

    ;; Return a copy of a given instruction x,
    ;; but replacing each constant c in the instruction x with (change c).
    (define (change-inst x change) ?)
    
    ;; Return a list of copies of a given instruction x,
    ;; but replacing each constant c in the instruction x with
    ;; one of the values from (change c).
    ;; Because (change c) returns a list of values instead of a value,
    ;; this method has to return all possible unique copies of x.
    (define (change-inst-list x change) ?)

    ))
