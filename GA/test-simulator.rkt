#lang racket

(require "GA-parser.rkt" "GA-printer.rkt" "GA-machine.rkt" "GA-validator.rkt"
         "GA-simulator-racket.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define simulator-racket (new GA-simulator-racket% [machine machine]))

(define code (send parser ast-from-string 
                   "dup push or and pop or"))
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)


;(define input
;   (cdar (send machine get-states-from-file "data-fff/inputs")))
;(define output
;  (send simulator-racket interpret encoded-code input #:dep #f))
;(send machine display-state output)

(define input (default-state machine 0 (thunk 0) [s 0] [t 0]))
(send machine display-state 
      (send simulator-racket interpret encoded-code input #:dep #f))

;(define live-in
;  (send solver get-live-in encoded-code (constraint (data 0) s t memory) 0))


#|
(define (sym-input)
  (define-symbolic* input number?)
   input)
(define state (default-state machine 1 sym-input))
(send machine display-state (send simulator-rosette interpret encoded-sketch state #:dep #f))
|#

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
