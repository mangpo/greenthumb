#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt" "GA-simulator-rosette.rkt" 
         "GA-parser.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine% [config 4]))
(define printer (new GA-printer% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [printer printer] [simulator simulator-rosette]))

(define code
(send parser ir-from-string 
      "325 325 325 b! @b"))

(define sketch (send parser ir-from-string 
      "0 0"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))

(define ce
  (send validator counterexample encoded-code encoded-sketch 
        (send machine output-constraint '() 1 0)
        #f
        ;#:assume (constrain-stack 
        ;          machine '((<= . 7) (<= . 7) (<= . 7)))
        ))

(if ce
    (pretty-display `(ce ,ce))
    (pretty-display "No counterexample."))
;(send machine display-state (send simulator-rosette interpret encoded-code ce))
;(send machine display-state (send simulator-rosette interpret encoded-sketch ce))

#|
(define t (current-seconds))
(with-handlers* 
 ([exn:fail? 
   (lambda (e) (pretty-display (exn-message e)))])
 (send solver synthesize-from-sketch encoded-code encoded-sketch
       (constraint s t r) 1))
(pretty-display `(time ,(- (current-seconds) t)))|#
