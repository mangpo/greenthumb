#lang s-exp rosette

(require "../inst.rkt"
         "$-parser.rkt" "$-machine.rkt" "$-printer.rkt"
         "$-simulator-rosette.rkt" 
         "$-simulator-racket.rkt"
         "$-validator.rkt"
         "$-symbolic.rkt"
         ;;"$-stochastic.rkt"
         ;;"$-forwardbackward.rkt" "$-enumerator.rkt" "$-inverse.rkt"
         )

(define parser (new $-parser%))
(define machine (new $-machine% [config ?]))
(define printer (new $-printer% [machine machine]))
(define simulator-racket (new $-simulator-racket% [machine machine]))
(define simulator-rosette (new $-simulator-rosette% [machine machine]))
(define validator (new $-validator% [machine machine] [simulator simulator-rosette]))


(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))

(define code
(send parser ir-from-string "
code here
"))

(define sketch
(send parser ir-from-string "
?
"))


(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))

(send validator adjust-memory-config
      (vector-append encoded-prefix encoded-code encoded-postfix))

;; Phase 0: create constraint (live-out in program state format).
;; constraint should be a program state that contains #t and #f,
;; where #t indicates the corresponding element in the program state being live.
(define constraint (progstate ?))

;; Phase A: create symbolic search (step 4)
(define symbolic (new $-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))

(send symbolic synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

;; Phase B: create stochastic search (step 5)
#;(define stoch (new $-stochastic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]
                      [syn-mode #t] ;; #t = synthesize, #f = optimize mode
                      ))
#;(send stoch superoptimize encoded-code 
      constraint ;; live-out
      "./driver-0" 
      3600 ;; time limit in seconds
      #f ;; size limit
      )


;; Phase C: create enumerative search (step 6)
#;(define backward (new $-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% $-inverse%]
                      [enumerator% $-enumerator%]
                      [syn-mode `linear]))
#;(send backward synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )
