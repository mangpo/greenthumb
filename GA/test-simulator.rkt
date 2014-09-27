#lang s-exp rosette

(require "GA-parser.rkt" "GA-printer.rkt" "GA-machine.rkt" 
         "GA-simulator-racket.rkt" "GA-solver.rkt" "GA-stochastic.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define simulator (new GA-simulator-racket% [machine machine]))
(define solver (new GA-solver% [machine machine] [printer printer]))
(define stochastic (new GA-stochastic% [machine machine] [printer printer] [syn-mode #t]))

(define code2 (send parser ast-from-string "222 left b! !b right a! @"))
(define code (send parser ast-from-string "7 nop + 262136 and 0 a! !"))
(define sketch (send parser ast-from-string "7 nop + 262136 and"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-struct encoded-code)

(define (sym-input)
  (define-symbolic* input number?)
  input)
(define state (default-state machine 1 (lambda () (random 100))))
(send machine display-state state)

(define final-state (send simulator interpret encoded-code state #:dep #t))
(define final-state2 (send simulator interpret encoded-sketch state))
(send machine display-state final-state)
(send stochastic correctness-cost final-state final-state2 (constraint t memory))
