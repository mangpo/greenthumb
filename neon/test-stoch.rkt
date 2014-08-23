#lang s-exp rosette

(require "neon-solver.rkt" "neon-stochastic.rkt" 
         "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt")

(define parser (new neon-parser%))
(define machine (new neon-machine%)) ;; TODO set-config
(define printer (new neon-printer% [machine machine]))
(define stochastic (new neon-stochastic% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
vmov d4, d9
vmov d4, d9
vmov d4, d9
vmov d4, d9
vmov d4, d9
vmov d4, d9
vmov d4, d9
vmla.s16 d3, d0, d1[2]
"))

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)

(send stochastic superoptimize encoded-code 
      (constraint machine [dreg 3] [rreg] [mem-all]) 
      #t "output/driver-0" 60 #f)