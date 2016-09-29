#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt" "../inst.rkt"
         "GA-simulator-racket.rkt" "GA-simulator-rosette.rkt"
         "GA-symbolic.rkt" "GA-stochastic.rkt" "GA-forwardbackward.rkt"
         "GA-inverse.rkt" "GA-enumerator.rkt")


(define parser (new GA-parser%))
(define machine (new GA-machine% [config #f]))

(define printer (new GA-printer% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [simulator simulator-rosette]))
(define symbolic (new GA-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))

(define code
(send parser ir-from-string "
dup drop up a! @ !
"))
;; expect: 325(up) a! @ !
;; okay: dup drop up a! @ @

(define encoded-code (send printer encode code))

(define sketch (for/vector ([i 4]) (send symbolic gen-sym-inst)))
(define constraint (send machine output-constraint '((data . 2) memory a)))

;; Synthesize
(send symbolic synthesize-from-sketch encoded-code sketch constraint)

;; Verify
(define code2 (send parser ir-from-string 
"up a! @ !")) ;; output from synthesize
(define encoded-code2 (send printer encode code2))

#;(define ce
  (send validator counterexample encoded-code encoded-code2 
        constraint))

#;(if ce
    (pretty-display `(ce ,ce))
    (pretty-display "No counterexample."))