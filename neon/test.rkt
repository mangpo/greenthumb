#lang racket

(require "parser.rkt" "print.rkt" "machine.rkt" "solver-support.rkt" "interpret.rkt")

(define code
(ast-from-string "
VEXT.16 d5, d3, d4, #1
VEXT.16 d6, d3, d4, #2
VEXT.16 d7, d3, d4, #3
VMLAL.s16 q0, d3, d2[0]
VMLAL.S16 q0, d5, d2[1] ; 1 cycle (DP)
 VMLAL.S16 q0, d6, d2[2] ; 1 cycle (DP)
 VMLAL.S16 q0, d7, d2[3] ; 1 cycle (DP)
vandi d9, 3
")) ;; TODO debug

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
               (make-vector nregs-r 0) (make-vector (* nmems 8) 0)))
(display-state state)
(display-state (interpret encoded-code state))