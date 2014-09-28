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
(define code (send parser ast-from-string "2 b! @b 3 b! !b 1 b! @b 2 b! !b"))
(define sketch (send parser ast-from-string "3 2/ nop a! @+ @ b! !"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(send printer print-struct encoded-code)

;; (define (sym-input)
;;   (define-symbolic* input number?)
;;   input)
;; (define state (default-state machine 1 (lambda () (random 100))))
;; (send machine display-state state)
(define inputs (map cdr (send machine get-states-from-file "data-ex/inputs")))

(define states1 
  (for/list ([input inputs])
    (send simulator interpret encoded-code input #:dep #t)))
(define states2 
  (for/list ([input inputs])
    (send simulator interpret encoded-sketch input #:dep #f)))


;(send stochastic correctness-cost final-state final-state2 (constraint t memory))

(define total-cost
  (for/sum ([state1 states1]
            [state2 states2])
    (let ([cost
           (send stochastic correctness-cost state1 state2 
                 (constraint t memory))])
      #|(pretty-display "expect=")
      (send machine display-state state1)
      (pretty-display "output=")
      (send machine display-state state2)|#
      (pretty-display `(cost ,cost))
      cost)))

(pretty-display `(total-cost ,total-cost))
