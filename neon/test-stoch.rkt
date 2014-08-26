#lang s-exp rosette

(require "neon-solver.rkt" "neon-stochastic.rkt" 
         "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt")

(define parser (new neon-parser%))
(define machine (new neon-machine%)) ;; TODO set-config
(send machine set-config (list 10 4 4))
(define printer (new neon-printer% [machine machine]))
(define stochastic (new neon-stochastic% [machine machine] [printer printer]))
(send stochastic print-mutation-info)
;(raise "done")

(define code
(send parser ast-from-string "
vst1.32	{d6,d7}, [r2]
vst1.32	{d6,d7}, [r2]
vst1.32	{d6,d7}, [r2]
"))

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)

(send stochastic superoptimize encoded-code 
      (constraint machine [dreg] 
                  [rreg] [mem-all]) 
      #t "output/driver-0" 120 #f)