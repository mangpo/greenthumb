#lang racket

(require "../printer.rkt" 
         "../inst.rkt")

(provide $-printer%)

(define $-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (override encode-inst decode-inst print-syntax-inst)

    ;; Print in the assembly format.
    ;; x: string IR
    (define (print-syntax-inst x [indent ""]) ?)

    ;; Convert an instruction x from string-IR to encoded-IR format.
    (define (encode-inst x) ?)

    ;; Convert an instruction x from encoded-IR to string-IR format.
    (define (decode-inst x) ?)

    ))
