#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt" "GA-simulator-rosette.rkt" 
         "GA-enumerative.rkt" "GA-symbolic.rkt" "GA-stochastic.rkt")

(define parser (new GA-parser%))
(define machine (new GA-machine%))
(send machine set-config 4)
(define printer (new GA-printer% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [printer printer] [simulator simulator-rosette]))
(define enum (new GA-enumerative% [machine machine] [printer printer] [parser parser]))
(define symbolic (new GA-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new GA-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))

(define prefix
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
1 2 3
"))

(define sketch
(send parser ast-from-string "
? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define t (current-seconds))
(send enum synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      encoded-prefix encoded-postfix
      (constraint r s t) 0 #f 36000)
#|(send stoch superoptimize encoded-code 
      (constraint r s t) ;; constraint
      #f ;; live-in
      "./driver-0" 3600 #f 0)|#
(pretty-display `(time ,(- (current-seconds) t)))
