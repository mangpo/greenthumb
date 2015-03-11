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
str r0, fp, -16
str r1, fp, -20
ldr r2, fp, -16
ldr r3, fp, -20
and r3, r2, r3
str r3, fp, -12
ldr r2, fp, -16
eor r3, r0, r1
"))

(define postfix
(send parser ast-from-string "
movls r0, 1
lslhi r0, r2, 48
"))

(define code
(send parser ast-from-string "
str r3, fp, -8
ldr r2, fp, -8
ldr r3, fp, -12
cmp r2, r3
"))

(define sketch
(send parser ast-from-string "
ldr r2, fp, -12
cmp r3, r2
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define a
(send parser ast-from-string "
add r0, r0, r0, lsl 827
"))
(define encoded-a (send printer encode a))
(send printer print-syntax (send printer decode encoded-a))

(define t (current-seconds))
#|(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      encoded-prefix encoded-postfix
      (constraint machine [reg 0] [mem 0]) #f #f 36000)|#
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem 0]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#
(pretty-display `(time ,(- (current-seconds) t)))
