#lang s-exp rosette

(require "GA-solver.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(define printer (new GA-printer% [machine machine]))
(define solver (new GA-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "up a! ! down b! @b 1 2 3"))


(define sketch
(send parser ast-from-string "up a! ! down b! @b 1 2 3"))


(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

(send solver counterexample encoded-code encoded-sketch (constraint (data 1) s t) 1)
