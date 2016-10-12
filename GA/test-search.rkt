#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt" "../inst.rkt"
         "GA-simulator-racket.rkt" "GA-simulator-rosette.rkt"
         "GA-symbolic.rkt" "GA-stochastic.rkt" "GA-forwardbackward.rkt"
         "GA-inverse.rkt" "GA-enumerator.rkt")


(define parser (new GA-parser%))
(define machine (new GA-machine% [config 1]))

(define printer (new GA-printer% [machine machine]))
(define simulator-racket (new GA-simulator-racket% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [simulator simulator-rosette]))

(define symbolic (new GA-symbolic% [machine machine] [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define stoch (new GA-stochastic% [machine machine] [printer printer] [parser parser] [syn-mode #t]
                   [validator validator] [simulator simulator-racket]))
(define backward (new GA-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% GA-inverse%]
                      [enumerator% GA-enumerator%]
                      [syn-mode `partial1]))

(define prefix 
(send parser ir-from-string "
0 a! !+ 0 
"))

(define postfix
(send parser ir-from-string "
2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ push drop pop dup over - 1 + 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ over - and + push drop pop
"))

(define code
(send parser ir-from-string "
0 b! @b 2/ 2/ 2/ 2/
"))


(define sketch
(send parser ir-from-string "
? ? ? ? ? ?
")) 
;[drop pop a ] 325 9 2/ b! a! ! !b @b 2* 2* 325 b! @b 3  [and + ]
; drop pop a ] 3 325 b! a! !b 2* 2* 325 b! @b 3 [and +
; drop pop a 325 a! ! ] 2* 2* @+ 3 and +
; opt: drop pop a 325 a! ! 2* 2* @ 3 and or 

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))

(define t1 (current-seconds))
(define f
  (send backward synthesize-window
        encoded-code ;; spec
        encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (constraint r s t) 1 11 3600
        ;#:assume (constrain-stack 
        ;          machine '((<= . 65535) (<= . 65535) (<= . 65535)))
        ))
(define t2 (current-seconds))
(pretty-display `(time ,(- t2 t1)))
#|(send stoch superoptimize encoded-code 
      (constraint machine [reg 0] [mem]) ;; constraint
      (constraint machine [reg 0] [mem]) ;; live-in
      "./driver-0" 3600 #f)|#

;(require profile)
;(profile-thunk f)
