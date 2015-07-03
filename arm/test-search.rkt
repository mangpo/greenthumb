#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "../ast.rkt"
         "arm-symbolic.rkt" "arm-stochastic.rkt" "arm-forwardbackward.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 9 9))

(define printer (new arm-printer% [machine machine]))
(define validator (new arm-validator% [machine machine]))

(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))
(define backward (new arm-forwardbackward% [machine machine] [printer printer] [parser parser]))

(define prefix 
(send parser ast-from-string "
str r0, fp, -24
str r1, fp, -28
str r2, fp, -32
str r3, fp, -36
ldr r2, fp, -24
ldr r3, fp, -36
cmp r2, r3
movne r3, 0
moveq r3, 1
rsb r3, r3, 0
str r3, fp, -20
ldr r2, fp, -28
ldr r3, fp, -36
eor r3, r2, r3
str r3, fp, -16
"))

(define postfix
(send parser ast-from-string "
str r3, fp, -12
ldr r2, fp, -32
ldr r3, fp, -36
eor r3, r2, r3
str r3, fp, -8
ldr r2, fp, -20
ldr r3, fp, -16
and r3, r2, r3
str r3, fp, -20
ldr r2, fp, -12
ldr r3, fp, -8
and r3, r2, r3
str r3, fp, -12
ldr r2, fp, -20
ldr r3, fp, -12
eor r3, r2, r3
str r3, fp, -20
ldr r2, fp, -20
ldr r3, fp, -36
eor r3, r2, r3
mov r0, r3
"))

(define code
(send parser ast-from-string "
ldr r2, fp, -24
ldr r3, fp, -28
cmp r2, r3
movne r3, 0
moveq r3, 1
rsb r3, r3, 0
"))


(define sketch
(send parser ast-from-string "
? ? ? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))


(define f
  (send backward synthesize-window
        encoded-code ;; spec
        encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint machine [reg 0] [mem]) #f #f 90)
  )
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

;(require profile)
;(profile-thunk f)
