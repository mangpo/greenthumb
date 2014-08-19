#lang s-exp rosette

(require "neon-solver.rkt" "neon-stochastic.rkt" 
         "neon-machine.rkt" "neon-printer.rkt"
         "parser.rkt")

(define machine (new neon-machine%)) ;; TODO set-config
(define printer (new neon-printer% [machine machine]))
(define solver (new neon-solver% [machine machine] [printer printer]))
(define stochastic (new neon-stochastic% [machine machine] [printer printer] [solver solver]))

(define code
(ast-from-string "
vld1 {d9}, [r0]
vld1 {d9}, [r0]
VMLAL.S16 q0, d3, d2[0] 
"))

(define encoded-code (send printer encode code))

(send stochastic superoptimize encoded-code 
      (constraint machine [dreg 0 1] [rreg] [mem-all])
      #t "foo" 10 1)