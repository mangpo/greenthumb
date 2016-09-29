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
      "0 a! push !+ !+ pop dup 1 b! @b and over 65535 or 0 b! @b and over - and + push drop pop"))

(define sketch (send parser ir-from-string 
      "a! over or dup a and or nop or"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))

(define ce
  (send validator counterexample encoded-code encoded-sketch 
        (send machine output-constraint '((data . 2)))
        #:assume (send machine constrain-stack 
                       '((<= . 65535) (<= . 65535) (<= . 65535)))
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
