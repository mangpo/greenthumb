#lang racket

(require "../parallel.rkt" "../compress.rkt"
         "neon-parser.rkt" "neon-meta.rkt" "neon-machine.rkt" "neon-printer.rkt"  "neon-solver.rkt" "neon-solver.rkt")

(define parser (new neon-parser%))
(define meta (new neon-meta%))
(define machine (new neon-machine%))
(define printer (new neon-printer% [machine machine]))
(define compress (new compress% [machine machine]))
(define solver (new neon-solver% [machine machine] [printer printer]))
(define parallel (new parallel% [meta meta] [parser parser] [machine machine] [printer printer] [compress compress] [solver solver]))

(define code
(send parser ast-from-string "
 VMOV d3, d9 ; 1 cycle (LSBP)
 VMOV d3, d9 ; 1 cycle (LSBP)
 VMOV d3, d9 ; 1 cycle (LSBP)
"))

#|
 VLD1.16 {d4}, [r2]! ; 2 cycles (LSBP)
 VLD1.16 {d2}, [r1]! ; 2 cycles (LSBP)
 VEXT.16 d5, d3, d4, #1 ; 1 cycle (LSBP)
 VEXT.16 d6, d3, d4, #2 ; 1 cycle (LSBP)
 VEXT.16 d7, d3, d4, #3 ; 1 cycle (LSBP)
 VMLAL.S16 q0, d3, d2[0] ; 1 cycle (DP)
 VMLAL.S16 q0, d5, d2[1] ; 1 cycle (DP)
 VMLAL.S16 q0, d6, d2[2] ; 1 cycle (DP)
 VMLAL.S16 q0, d7, d2[3] ; 1 cycle (DP)
 VMOV d9, d4 ; 1 cycle (LSBP)|#

(send parallel optimize code (list (list 0 1 3) (list 1 2)) #t #:time-limit 36000)