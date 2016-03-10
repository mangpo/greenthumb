#lang s-exp rosette

(require "../inst.rkt"
         "$-parser.rkt" "$-machine.rkt" "$-printer.rkt"
         "$-simulator-rosette.rkt" 
         ;;"$-simulator-racket.rkt"
         "$-validator.rkt"
         "$-symbolic.rkt"
         ;;"$-stochastic.rkt"
         ;;"$-forwardbackward.rkt" "$-enumerator.rkt" "$-inverse.rkt"
         )

(define parser (new $-parser%))
(define machine (new $-machine% [config ?]))
(define printer (new $-printer% [machine machine]))
;;(define simulator-racket (new $-simulator-racket% [machine machine]))
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


;; Phase 0: create constriant (live-out)
(define constraint ?)

;; Phase A: create symbolic search (step 4)
(define symbolic (new $-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))

(send symbolic synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; extra parameter (not use most of the time)
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
      ? ;; live-in
      "./driver-0" 
      3600 ;; time limit in seconds
      #f   ;; extra parameter (not use most of the time)
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
      #f ;; extra parameter (not use most of the time)
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )
