#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "../ast.rkt"
         "arm-symbolic.rkt" "arm-stochastic.rkt" "arm-forwardbackward.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 3 0 0))

(define printer (new arm-printer% [machine machine]))
(define validator (new arm-validator% [machine machine]))

(define symbolic (new arm-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new arm-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))
(define backward (new arm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [syn-mode `partial1]))

(define prefix 
(send parser ast-from-string "
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
        eor     r2, r1, r0
        and     r0, r1, r0
        add     r0, r0, r2, asr #1
add r0, r0, 11
"))


(define sketch
(send parser ast-from-string "
? ? ?
"))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))


(define f
  (send backward synthesize-window
        encoded-code ;; spec
         3;;encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint machine [reg 0] [mem]) #f #f 3600)
  )
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

;(require profile)
;(profile-thunk f)
