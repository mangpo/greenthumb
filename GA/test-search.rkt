#lang s-exp rosette

(require "GA-validator.rkt" "GA-machine.rkt" "GA-printer.rkt"
         "GA-parser.rkt" "../inst.rkt"
         "GA-simulator-racket.rkt" "GA-simulator-rosette.rkt"
         "GA-symbolic.rkt" "GA-stochastic.rkt" "GA-forwardbackward.rkt"
         "GA-inverse.rkt" "GA-enumerator.rkt")


(define parser (new GA-parser%))
(define machine (new GA-machine% [config #f]))

(define printer (new GA-printer% [machine machine]))
(define simulator-racket (new GA-simulator-racket% [machine machine]))
(define simulator-rosette (new GA-simulator-rosette% [machine machine]))
(define validator (new GA-validator% [machine machine] [simulator simulator-rosette]))

(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))

(define code
(send parser ir-from-string "
dup drop left a! !+ @
"))


(define sketch
(send parser ir-from-string "
? ? ? ?
"))
;[drop pop a ] 325 9 2/ b! a! ! !b @b 2* 2* 325 b! @b 3  [and + ]
; drop pop a ] 3 325 b! a! !b 2* 2* 325 b! @b 3 [and +
; drop pop a 325 a! ! ] 2* 2* @+ 3 and +
; opt: drop pop a 325 a! ! 2* 2* @ 3 and or 

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))

;; Phase 0: create constraint (live-out)
(define livein (send machine output-constraint '(a) 4 1))
(define constraint (send machine output-constraint '(a) 1 0))

(send machine analyze-opcode encoded-prefix encoded-code encoded-postfix)
(send machine reset-arg-ranges)
(send machine analyze-args encoded-prefix encoded-code encoded-prefix
      livein constraint)

;; Phase A: create symbolic search (step 4)
(define symbolic (new GA-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))

#;(send symbolic synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; extra parameter (not use most of the time)
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

;; Phase B: create stochastic search (step 5)
(define stoch (new GA-stochastic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-racket]
                      [syn-mode #t] ;; #t = synthesize, #f = optimize mode
                      ))
#;(send stoch superoptimize encoded-code 
      constraint ;; live-out
      livein
      "./driver-0" 
      3600 ;; time limit in seconds
      #f   ;; extra parameter (not use most of the time)
      )


;; Phase C: create enumerative search (step 6)
(define backward (new GA-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% GA-inverse%]
                      [enumerator% GA-enumerator%]
                      [syn-mode `linear]))
(send backward synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; extra parameter (not use most of the time)
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#;(define f
  (send backward synthesize-window
        encoded-code ;; spec
        encoded-sketch ;; sketch = spec in this case
        encoded-prefix encoded-postfix
        (send machine output-constraint '() 2 0)
        1 11 3600
        ;#:assume (constrain-stack 
        ;          machine '((<= . 65535) (<= . 65535) (<= . 65535)))
        ))
#;(send stoch superoptimize encoded-code 
      (send machine output-constraint '() 2 0) ;; constraint
      (send machine output-constraint '(a) 4 1) ;; live-in
      "./driver-0" 3600 #f)

;(require profile)
;(profile-thunk f)
