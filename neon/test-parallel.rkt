#lang racket

(require "main.rkt" "neon-parser.rkt")

(define parser (new neon-parser%))

(define code
(send parser ast-from-string "
 VMOV d3, d9 ; 1 cycle (LSBP)
 VMOV d3, d9 ; 1 cycle (LSBP)
VMLAL.S16 q0, d7, d2[3] ; 1 cycle (DP)
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

(optimize code (list (list 0 1) (list)) #t #:time-limit 3600 #:cores 12)