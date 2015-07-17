#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "../ast.rkt"
         "arm-symbolic.rkt" "arm-stochastic.rkt" "arm-forwardbackward.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 6 6))

(define printer (new arm-printer% [machine machine]))
(define validator (new arm-validator% [machine machine]))

(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))
(define backward (new arm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [syn-mode `partial1]))

(define prefix 
(send parser ast-from-string "
str r0, fp, -16
str r1, fp, -20
str r2, fp, -24
ldr r2, fp, -16
ldr r3, fp, -24
asr r2, r2, r3
ldr r3, fp, -16
eor r3, r2, r3
rsb r2, r3, r3, lsl 1
ldr r3, fp, -20
and r3, r2, r3
sub r2, r3, 0
str r3, fp, -12
ldr r3, fp, -24
lsl r3, r2, r3
"))

(define postfix
(send parser ast-from-string "
ldr r3, fp, -16
eor r3, r2, r3
mov r0, r3
"))

(define code
(send parser ast-from-string "
str r3, fp, -8
ldr r2, fp, -8
ldr r3, fp, -12
eor r2, r2, r3
"))


(define sketch
(send parser ast-from-string "
? ?
"))
;; kodkod: 117, 123
;; z3: > 8 min
;; cvc4: 133, 122

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define t (current-seconds))
;(define-values (a b)
  (send backward synthesize-window
        encoded-code ;; spec
        2 ;;encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint machine [reg 0] [mem]) #f #f 3600)
 ; )
(pretty-display `(time ,(- (current-seconds) t)))
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

;(require profile)
;(profile-thunk f)
