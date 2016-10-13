#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "../inst.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         "arm-symbolic.rkt" "arm-stochastic.rkt" 
         "arm-forwardbackward.rkt" "arm-enumerator.rkt" "arm-inverse.rkt"
         )

(define parser (new arm-parser%))
(define machine (new arm-machine% [config 6]))

(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [simulator simulator-rosette]))

(define symbolic (new arm-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define stoch (new arm-stochastic% [machine machine] [printer printer]
                   [validator validator] [simulator simulator-racket]
                   [parser parser] [syn-mode #t]))
(define backward (new arm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% arm-inverse%]
                      [enumerator% arm-enumerator%]
                      [syn-mode `partial1]))

(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))

(define code
(send parser ir-from-string "
        str     r0, [r4, #-24]
        ldr     r3, [r4, #-24]
        rsb     r3, r3, #0
        mov     r2, r3
        ldr     r3, [r4, #-24]
        and     r3, r2, r3
        str     r3, [r4, #-20]
        ldr     r2, [r4, #-24]
        ldr     r3, [r4, #-20]
        add     r3, r2, r3
        str     r3, [r4, #-16]
        ldr     r2, [r4, #-24]
        ldr     r3, [r4, #-16]
        eor     r3, r2, r3
        str     r3, [r4, #-12]
        ldr     r0, [r4, #-12]
        ldr     r1, [r4, #-20]
        bl      __aeabi_uidiv
        mov     r3, r0
        str     r3, [r4, #-8]
        ldr     r3, [r4, #-8]
        mov     r3, r3, lsr #2
        str     r3, [r4, #-8]
        ldr     r2, [r4, #-8]
        ldr     r3, [r4, #-16]
        orr     r3, r2, r3
        mov     r0, r3
"))


(define sketch
(send parser ir-from-string "
? ?
"))
;; p13 -O0
;; z3: >5 min, java: 12 s
;; rsb r1, r0, r0, lsr 1
;; orr r0, r0, r1, asr 31
;; p25 -O3
;; z3: >5 min, java: 9 s

(define constraint (send printer encode-live '(0)))

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))

(send validator adjust-memory-config (vector-append encoded-prefix encoded-code encoded-postfix))

#;(send symbolic synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      3 ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

#;(send stoch superoptimize encoded-code 
      constraint ;; constraint
      "./driver-0" 3600 #f)

(send backward synthesize-window
      encoded-code ;; spec
      encoded-sketch ;; sketch => start from searching from length 1, number => only search for that length
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )
