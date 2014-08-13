#lang racket

(require "parser.rkt" "print.rkt" "machine.rkt" "solver-support.rkt" "interpret.rkt")

(define code
(ast-from-string "
vld1 {d0}, [r0]
"))

;(define code
;  (ast-from-string "?"))

(define encoded-code (encode code #f))

(print-struct code)
(print-struct encoded-code)

;(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
(define state (progstate 
               (vector 
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                1 0 95 4 156 136 95 2
                255 250 153 146 166 201 49 75
                170 50 212 200 49 61 2 26
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                255 255 255 255 255 255 255 255
                27 1 0 0 0 0 0 0
                )
               (vector 0 0 0 0)
               (list->vector (range (* 8 nmems)))
               0))
(display-state state)
(display-state (interpret encoded-code state))