#lang s-exp rosette

(require "GA-parser.rkt" "GA-printer.rkt" "GA-machine.rkt" "GA-validator.rkt"
         "GA-simulator-racket.rkt" "GA-simulator-rosette.rkt"
         "GA-symbolic.rkt"
         "../memory-racket.rkt" "../queue-racket.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine% [config #f] [bitwidth 4]))
(define printer (new GA-printer% [machine machine]))
(define simulator-racket (new GA-simulator-racket% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [printer printer] [simulator simulator-rosette]))

(define code1 (send parser ir-from-string 
                   "2 b! dup !b a! @+ @ @b b! - @b + -"))
(define code2 (send parser ir-from-string 
                   "2 b! dup !b a! @+ @+ - over + -"))
(define encoded-code1 (send printer encode code1))
(define encoded-code2 (send printer encode code2))

;(define input
;   (cdar (send machine get-states-from-file "data-fff/inputs")))
;(define output
;  (send simulator-racket interpret encoded-code input))
;(send machine display-state output)

(define (func-rand #:min [min #f] #:max [max #f] #:const [const #f])
  (if const const (random 10)))

(define (func-sym #:min [min #f] #:max [max #f] #:const [const #f])
  (define-symbolic* input number?)
  (if const const input))

(define input (send machine vector->progstate
                    (vector 0 0 0 0 -4
                            '#(#f #f #f #f #f #f #f #f)
                            '#(#f #f #f #f #f #f #f #f)
                            (new memory-racket%)
                            (new queue-in-racket% [queue (vector -8 -8 0 #f)])
                            (new queue-out-racket%))))
(define output1 (send simulator-rosette interpret encoded-code1 input))
(define output2 (send simulator-rosette interpret encoded-code2 input output1))

#|
(send validator assert-state-eq output1 output2 (send machine output-constraint '() 1 0))

(pretty-display "Phase C: interpret concrete program with symbolic inputs.")

(define symbolic (new GA-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define encoded-code-sym
  (for/vector ([i 1])
    (send symbolic gen-sym-inst)))

(define input-state-sym (send machine get-state func-sym))
(define output-sym (send simulator-rosette interpret encoded-code-sym input-state-sym))
(newline)
|#