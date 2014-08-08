#lang racket

(require "parser.rkt" "print.rkt" "machine.rkt" "solver-support.rkt" "interpret.rkt")

(define code
(ast-from-string "
vmla.i32 d0, d2, d4
"))

(define encoded-code (encode code #f))

(print-struct code)
(print-struct encoded-code)

;(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
(define state (progstate 
               (vector 
                219 137 151 253 247 37 138 73 
                112 171 90 27 41 246 116 61
                145 230 19 80 13 177 36 55 
                0 0 0 0 0 0 0 0 
                31 144 175 242 76 202 214 199)
               (make-vector nregs-r 0) (make-vector (* nmems 8) 0)))
(display-state state)
(display-state (interpret encoded-code state))