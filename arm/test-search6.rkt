#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt" 
         "arm-enumerative.rkt" "arm-symbolic.rkt" "arm-stochastic.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 3 4))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer] [simulator simulator-rosette]))
(define enum (new arm-enumerative% [machine machine] [printer printer] [parser parser]))
(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))

(define prefix
(send parser ast-from-string "
mov r1, 21845
movt r1, 21845
mov r2, r0, asr 1
and r2, r1, r2
rsb r2, r2, r0
rev16 r1, r2
mov r2, 13107
movt r2, 13107
and r2, r1, r2
str r2, fp, -12
mov r2, r1, lsr 2
movt r1, 13107
movw r1, 13107
and r2, r1, r2
ldr r1, fp, -12
add r2, r1, r2, lsl 0
mov r1, r2, asr 4
add r2, r1, r2, asr 0
movw r1, 3855
movt r1, 3855
and r2, r1, r2
"))

(define postfix
(send parser ast-from-string "
mov r0, r2
"))

(define code
(send parser ast-from-string "
add r2, r2, r2, asr 16
add r2, r2, r2, lsr 8
and r2, r2, 63
"))

;; rev r1, r2
;; add r2, r1, r2
;; rev16 r1, r2
;; add r2, r1, r2
;; and r2, r2, 63

(define sketch
(send parser ast-from-string "
add r2, r2, r2, asr 16
add r2, r2, r2, lsr 8
and r2, r2, 63
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
