#lang racket

(require "parser.rkt" "print.rkt" "machine.rkt" "solver-support.rkt" "interpret.rkt")

(define code
(ast-from-string "
vmlal.u16 q0, d3, d2[0]
"))

;(define code
;  (ast-from-string "?"))

(define encoded-code (encode code))

(print-struct code)
(print-struct encoded-code)

;(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
(define state (progstate 
               (vector 
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                71 190 0 0 0 0 0 0
                225 77 15 108 0 0 0 0
                170 50 212 200 49 61 2 26
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                0 0 0 0 0 0 0 0
                255 255 255 255 255 255 255 255
                27 1 0 0 0 0 0 0
                )
               (make-vector nregs-r 0)
               (list->vector (range (* 8 nmems)))
               0))
(display-state state)
(display-state (interpret encoded-code state))