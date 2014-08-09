#lang s-exp rosette

(require "../solver.rkt"
         "parser.rkt" "machine.rkt" "solver-support.rkt")


(define code
(ast-from-string "
VEXT.16 d5, d3, d4, #1
")) ;; TODO debug


(define sketch
(ast-from-string "
VEXT.16 d5, d3, d4, #1
"))

(define encoded-code (encode code #f))
(define encoded-sketch (encode sketch #f))
(counterexample encoded-code encoded-sketch (constraint [dreg 5] [rreg] [mem-all]))