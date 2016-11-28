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


;[0 a! !+ 0 ]
;0 b! @b 2/ 2/ 2/ 2/
;[2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ push drop pop dup over - 1 + 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ over - and + push drop pop ]
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
")) ; dup ! a! 2 @+ @ 

(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))
(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))

;; Phase 0: create constraint (live-out)
(define livein (send machine output-constraint '(a) 4 1))
(define constraint (send machine output-constraint '((data . 2) (return . 1))))
;;(define precond (send machine constrain-stack '((<= . 65535) (<= . 65535) (<= . 65535))))

(send validator adjust-memory-config
      (vector-append encoded-prefix encoded-code encoded-postfix))
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
      #f
      )


;; Phase C: create enumerative search (step 6)
(define backward (new GA-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% GA-inverse%]
                      [enumerator% GA-enumerator%]
                      [syn-mode `linear]))
(define t1 (current-seconds))
(send backward synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      ;;#:assume precond
      )

(define t2 (current-seconds))
(pretty-display `(time ,(- t2 t1)))
