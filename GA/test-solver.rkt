#lang s-exp rosette

(require "GA-solver.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(define printer (new GA-printer% [machine machine]))
(define solver (new GA-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string 
      "2 b! @b 3 b! !b 1 b! @b 2 b! !b"))

(define sketch (send parser ast-from-string "_ _ _ _ _ _ _ _"))
;(define sketch (send parser ast-from-string "dup push or and pop or"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

#|
(send solver counterexample encoded-code encoded-sketch 
      (constraint t)
      0
      #:assume (constrain-stack machine '((<= . 65535) (<= . 65535) (<= . 65535))))|#


(send solver synthesize-from-sketch encoded-code encoded-sketch
      (constraint s t memory) 0)