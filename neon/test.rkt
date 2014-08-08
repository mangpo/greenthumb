#lang racket

(require "parser.rkt" "print.rkt" "machine.rkt" "solver-support.rkt" "interpret.rkt")

(define code
(ast-from-string "
VEXT.16 d0, d3, d4, #1
"))

(define encoded-code (encode code #f))

(print-struct code)
(print-struct encoded-code)

(define state (struct-copy progstate (default-state (random 10)) [rregs (vector 0 0 0 0)]))
(display-state state)
(display-state (interpret encoded-code state))