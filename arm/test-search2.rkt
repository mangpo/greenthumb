#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt" 
         "arm-enumerative.rkt" "arm-symbolic.rkt" "arm-stochastic.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 4 5))
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
ldr r3, fp, -20
and r3, r2, r3
str r3, fp, -12
eor r3, r2, r1, lsl 0
ldr r2, fp, -12
cmpcs r3, r2
movls r0, 1
lslhi r0, r2, 32
"))

(define code
(send parser ast-from-string "
str r0, fp, -16
str r1, fp, -20
ldr r2, fp, -16
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
      (constraint machine [reg 0] [mem]) #f #f 36000)
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem 0]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#
(pretty-display `(time ,(- (current-seconds) t)))
