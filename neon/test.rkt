#lang racket

(require "parser.rkt" "print.rkt" "machine.rkt" "solver-support.rkt" "interpret.rkt")

(define code
(ast-from-string "
 VEXT.16 d0, d1, d2, #1 ; 1 cycle (LSBP)
"))

;(define code
;  (ast-from-string "?"))

(define encoded-code (encode code))

(print-struct code)
(print-struct encoded-code)

;(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
;(define state (default-state 0))
(define state (progstate 
               (vector 
                0 0 0 0 0 0 0 0
                0 1 2 3 4 5 6 7
                8 9 11 22 33 44 55 66
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