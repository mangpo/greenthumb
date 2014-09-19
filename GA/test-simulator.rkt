#lang s-exp rosette

(require "GA-parser.rkt" "GA-printer.rkt" "GA-machine.rkt" 
         "GA-simulator-rosette.rkt" "GA-solver.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 1)
(define printer (new GA-printer% [machine machine]))
(define simulator (new GA-simulator-rosette% [machine machine]))
(define solver (new GA-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "222 left b! !b right a! @"))

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)

(define (sym-input)
  (define-symbolic* input number?)
  input)
(define state (default-state machine 1 (lambda () 0)))
(send machine display-state state)

(define final-state (send simulator interpret encoded-code state))
(send machine display-state-text final-state 0)
