#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt" 
         "arm-enumerative.rkt" "arm-symbolic.rkt" "arm-stochastic.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 8 1 0))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer] [simulator simulator-rosette]))
(define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))
(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))

(define prefix
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
mov r5, r5, lsl 11
sub r5, r5, 12
rsb r3, r2, r3
mla r3, r5, r7, r3
mla r1, r4, r1, r3
"))

(define code
(send parser ast-from-string "
mov r0, r2, lsl 2
rsb r5, r0, r2
mul r2, r6, r2
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
(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      encoded-prefix encoded-postfix
      (constraint machine [reg 1] [mem]) #f #f 36000)
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem 0]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#
(pretty-display `(time ,(- (current-seconds) t)))
