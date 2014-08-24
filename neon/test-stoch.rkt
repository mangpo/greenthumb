#lang s-exp rosette

(require "neon-solver.rkt" "neon-stochastic.rkt" 
         "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt")

(define parser (new neon-parser%))
(define machine (new neon-machine%)) ;; TODO set-config
(send machine set-config (list 24 1 1))
(define printer (new neon-printer% [machine machine]))
(define stochastic (new neon-stochastic% [machine machine] [printer printer]))
(send stochastic print-mutation-info)
;(raise "done")

(define code
(send parser ast-from-string "
vhadd.s16	q4, q0, q2
vhsub.s16	q5, q0, q2
vhadd.s16	q6, q1, q3
vhsub.s16	q7, q1, q3
vhsub.s16	q10, q4, q6
vhadd.s16	q8, q4, q6
vhadd.s16	d18, d10, d15
vhsub.s16	d19, d11, d14
vhsub.s16	d22, d10, d15
vhadd.s16	d23, d11, d14
vtrn.16	q8, q9
vtrn.16	q10, q11
vtrn.32	q8, q10
vtrn.32	q9, q11
"))

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)

(send stochastic superoptimize encoded-code 
      (constraint machine [dreg 0 1 2 3 4 5 6 7 16 17 18 19 20 21 22 23] 
                  [rreg] [mem-all]) 
      #t "output/driver-0" 60 #f)