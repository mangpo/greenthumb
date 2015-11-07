#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt" "../ast.rkt"
         "GA-symbolic.rkt" "GA-stochastic.rkt" "GA-forwardbackward.rkt")


(define parser (new GA-parser%))
(define machine (new GA-machine% [config 5]))

(define printer (new GA-printer% [machine machine]))
(define validator (new GA-validator% [machine machine]))

(define symbolic (new GA-symbolic% [machine machine] [printer printer] [parser parser]))
(define stoch (new GA-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]))
(define backward (new GA-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [syn-mode `partial1]))

(define prefix 
(send parser ast-from-string "
drop pop a 325 a! !
"))

(define postfix
(send parser ast-from-string "
"))

(define code
(send parser ast-from-string "
2* 2* @+ 3 and +
"))


(define sketch
(send parser ast-from-string "
? ? ? ? ? ?
")) 
;[drop pop a ] 325 9 2/ b! a! ! !b @b 2* 2* 325 b! @b 3  [and + ]
; drop pop a ] 3 325 b! a! !b 2* 2* 325 b! @b 3 [and +
; drop pop a 325 a! ! ] 2* 2* @+ 3 and +
; opt: drop pop a 325 a! ! 2* 2* @ 3 and or 

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define f
  (send backward synthesize-window
        encoded-code ;; spec
        encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint s t) 1 11 3600
        ;#:assume (constrain-stack 
        ;          machine '((<= . 65535) (<= . 65535) (<= . 65535)))
        ))
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

;(require profile)
;(profile-thunk f)
