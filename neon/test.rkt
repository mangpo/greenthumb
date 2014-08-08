#lang racket

(require "parser.rkt" "print.rkt" "machine.rkt" "solver-support.rkt" "interpret.rkt")

(define code
(ast-from-string "
vqmovn.u16 d0, q1
"))

(define encoded-code (encode code #f))

(print-struct code)
(print-struct encoded-code)

;(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
(define state (progstate 
               (vector 
                136 252 67 183 250 159 172 175 
                223 61 189 44 110 222 125 46 
                145 0 19 0 13 177 36 55 
                0 0 0 0 0 0 0 0 
                31 0 175 0 76 202 0 0)
               (make-vector nregs-r 0) (make-vector (* nmems 8) 0)))
(display-state state)
(display-state (interpret encoded-code state))