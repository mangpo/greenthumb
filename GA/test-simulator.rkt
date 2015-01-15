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

(define code (send parser ast-from-string 
                   "push over - push and pop pop and over 65535 or and or"))
(define sketch (send parser ast-from-string 
                     "1 2 3 4 5"))
(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-struct encoded-code)

(send printer get-constants encoded-sketch)

#|
(define input
   (cdar (send machine get-states-from-file "data-fff/inputs")))
(define output
  (send simulator interpret encoded-code input #:dep #f))
(send machine display-state output)
(send machine display-state
      (send machine vector->progstate (send machine progstate->vector output)))

(define live-in
  (send solver get-live-in encoded-code (constraint (data 0) s t memory) 0))
|#

;; (define (sym-input)
;;   (define-symbolic* input number?)
;;   input)
;; (define state (default-state machine 1 (lambda () (random 100))))
;; (send machine display-state state)
#|
(define inputs 
   (map cdr (send machine get-states-from-file "data-fff/inputs")))


(define states1 
  (for/list ([input inputs])
    (send simulator interpret encoded-code input #:dep #t)))

(define states2 
  (for/list ([input inputs])
    (send simulator interpret encoded-sketch input #:dep #f)))


;(send stochastic correctness-cost final-state final-state2 (constraint t memory))

(define total-cost
  (for/sum ([state1 states1]
            [state2 states2]
            [id (length states1)])
    (let ([cost
           (send stochastic correctness-cost state1 state2 
                 (constraint t a memory))])
      (pretty-display "expect=")
      (send machine display-state state1)
      (pretty-display "output=")
      (send machine display-state state2)
      (pretty-display `(cost ,cost ,id))
      cost)))

(pretty-display `(total-cost ,total-cost))|#
