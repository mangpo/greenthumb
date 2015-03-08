#lang s-exp rosette

(require "neon-stochastic.rkt" 
         "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt")

(define parser (new neon-parser%))
(define machine (new neon-machine%)) ;; TODO set-config
(send machine set-config (list 10 1 4))
(define printer (new neon-printer% [machine machine]))
(define stochastic (new neon-stochastic% [machine machine] [printer printer]
                        [syn-mode #t]))
;(send stochastic mutate-operand-specific `vst2 (vector (cons 2 (vector 3 4)) 0) 0)   
;(raise "done")

(define code
(send parser ast-from-string "
vorr q3, q0, q0
vld1 {d4,d5}, [r0]
vmov q1, q4
vswp d2, d3
vbsl q3, q1, q2
vst1.32	{d6,d7}, [r0]
"))

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)

(send stochastic superoptimize encoded-code 
      (constraint machine [dreg] 
                  [rreg] [mem-all]) 
      "test" 120 #f)