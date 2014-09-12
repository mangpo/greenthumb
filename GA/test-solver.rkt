#lang s-exp rosette

(require "GA-solver.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(define printer (new GA-printer% [machine machine]))
(define solver (new GA-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "a! push a and pop a - and over 65535 or and or"))

(define sketch (send parser ast-from-string "a! over or dup a _ _ _"))
;(define sketch (send parser ast-from-string "dup push or and pop or"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

#|
(send solver counterexample encoded-code encoded-sketch 
      (constraint t)
      0
      #:assume (constrain-stack machine '((<= . 65535) (<= . 65535) (<= . 65535))))|#


(send solver synthesize-from-sketch encoded-code encoded-sketch
      (constraint t) 0
      #:assume (constrain-stack machine '((<= . 65535) (<= . 65535) (<= . 65535))))
