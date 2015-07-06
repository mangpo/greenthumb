#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "../ast.rkt"
         "arm-symbolic.rkt" "arm-stochastic.rkt" "arm-forwardbackward.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 3 4))

(define printer (new arm-printer% [machine machine]))
(define validator (new arm-validator% [machine machine]))

(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))
(define backward (new arm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [syn-mode `partial1]))

(define prefix 
(send parser ast-from-string "
mov r2, r0, asr 1
movw r1, 21845
movt r1, 21845
and r2, r1, r2
rsb r2, r2, r0
str r2, fp, -16
movt r1, 13107
movw r1, 13107
"))

(define postfix
(send parser ast-from-string "
add r2, r1, r2
add r2, r2, r2, lsr 4
movt r1, 3855
movw r1, 3855
and r2, r1, r2
add r2, r2, r2, lsr 16
add r2, r2, r2, asr 8
and r0, r2, 63

"))

(define code
(send parser ast-from-string "
and r2, r1, r2
str r2, fp, -12
ldr r2, fp, -16
and r2, r1, r2, asr 2
ldr r1, fp, -12
"))


(define sketch
(send parser ast-from-string "
? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))


(define f
  (send symbolic synthesize-window
        encoded-code ;; spec
        encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint machine [reg 0] [mem]) #f #f 60)
  )
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

;(require profile)
;(profile-thunk f)
