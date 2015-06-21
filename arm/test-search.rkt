#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt"
         "arm-simulator-rosette.rkt" "arm-simulator-racket.rkt" 
         "arm-enumerative.rkt" "arm-symbolic.rkt" "arm-stochastic.rkt"
         "arm-database.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine% [bit 4]))
(send machine set-config (list 2 0 4))
(define machine-precise (new arm-machine% [bit 32]))
(send machine-precise set-config (list 2 0 4))

(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-racket-precise (new arm-simulator-racket% [machine machine-precise]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine]))
(define validator-precise (new arm-validator% [machine machine-precise]))

(define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))
(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))
(define db (new arm-database% [machine machine] [enum enum] 
                [simulator simulator-racket]
                [simulator-precise simulator-racket-precise]
                [printer printer] [parser parser] 
                [validator validator] [validator-precise validator-precise]))

(define prefix
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
	bic	r0, r0, r1
	cmp	r0, r1
	movls	r0, #0
	movhi	r0, #1
"))

(define sketch
(send parser ast-from-string "
? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))


(define (f)
   (send db synthesize-window2
         encoded-code ;; spec
         encoded-sketch ;; sketch = spec in this case
         encoded-prefix encoded-postfix
         (constraint machine [reg 0] [mem]) #f #f 3600)
   )
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

(f)
;(require profile)
;(profile-thunk f)
