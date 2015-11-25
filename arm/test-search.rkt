#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "../ast.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         "arm-symbolic.rkt" "arm-stochastic.rkt" 
         "arm-forwardbackward.rkt" "arm-enumerator.rkt" "arm-inverse.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine% [config (list 3 0 0)]))

(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer]
                       [simulator simulator-rosette]))

(define symbolic (new arm-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define stoch (new arm-stochastic% [machine machine] [printer printer]
                   [validator validator] [simulator simulator-racket]
                   [parser parser] [syn-mode #t]))
(define backward (new arm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% arm-inverse%]
                      [enumerator% arm-enumerator%]
                      [syn-mode `partial1]))

(define prefix 
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
	cmp	r0, r1
	movge	r0, r1
	ubfx	r0, r0, #15, #16
"))


(define sketch
(send parser ast-from-string "
? ? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))


  (send backward synthesize-window
        encoded-code ;; spec
        3 ;encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint machine [reg 0] [mem] [z #f]) #f #f 3600)
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

;(require profile)
;(profile-thunk f)
