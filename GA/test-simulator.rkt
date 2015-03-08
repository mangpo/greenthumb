#lang s-exp rosette

(require "GA-parser.rkt" "GA-printer.rkt" "GA-machine.rkt" "GA-validator.rkt"
         "GA-simulator-rosette.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [printer printer] [simulator simulator-rosette]))

(define code (send parser ast-from-string 
                   "2 b! dup a! !b @+ @ - over + -"))
(define sketch (send parser ast-from-string 
                     "? ? ? ? ? ?"))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))
(send printer print-struct encoded-code)

(send printer get-constants encoded-code)

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

(define (sym-input)
  (define-symbolic* input number?)
   input)
(define state (default-state machine 1 sym-input))
(send machine display-state (send simulator-rosette interpret encoded-sketch state #:dep #f))
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
