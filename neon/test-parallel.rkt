#lang racket

(require "main.rkt" "neon-parser.rkt")

(define parser (new neon-parser%))

(define code
(send parser ast-from-string "
 VMLAL.S16 q0, d3, d2[0] ; 1 cycle (DP)
 VEXT.16 d5, d3, d4, #1 ; 1 cycle (LSBP)
 VMLAL.S16 q0, d5, d2[1] ; 1 cycle (DP)
 VEXT.16 d5, d3, d4, #2 ; 1 cycle (LSBP)
 VMLAL.S16 q0, d5, d2[2] ; 1 cycle (DP)
 VEXT.16 d5, d3, d4, #3 ; 1 cycle (LSBP)
 VMLAL.S16 q0, d5, d2[3] ; 1 cycle (DP)
 VMOV d3, d4 ; 1 cycle (LSBP)
"))

(optimize code (list (list 0 1 3) (list)) #t #:time-limit 36000 #:cores 12)