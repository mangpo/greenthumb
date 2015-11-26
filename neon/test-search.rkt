#lang s-exp rosette

(require "neon-validator.rkt" "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt" "neon-inst.rkt" "neon-simulator-rosette.rkt" 
         "neon-symbolic.rkt" "neon-stochastic.rkt")

(define parser (new neon-parser%))
(define machine (new neon-machine%))
(send machine set-config (list 10 1 4))
(define printer (new neon-printer% [machine machine]))
(define simulator-rosette (new neon-simulator-rosette% [machine machine]))
(define validator (new neon-validator% [machine machine] [printer printer] [simulator simulator-rosette]))
(define symbolic (new neon-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new neon-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))

(define prefix
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
vst1.32	{d6,d7}, [r0]
"))

(define sketch
(send parser ast-from-string "
?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define t (current-seconds))
#|(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      encoded-prefix encoded-postfix
      (constraint machine [dreg] [rreg] [mem-all]) #f #f 36000)|#
(send stoch superoptimize encoded-code 
      (constraint machine [dreg] [rreg] [mem-all]) ;; constraint
      (constraint machine [dreg 6 7] [rreg] [mem-all]) ;; live-in
      "./driver-0" 3600 #f)
(pretty-display `(time ,(- (current-seconds) t)))
