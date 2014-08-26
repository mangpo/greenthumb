#lang racket

(require "main.rkt" "neon-parser.rkt")

(define parser (new neon-parser%))

(define code
(send parser ast-from-string "
vorr q3, q0, q0
vld1 {d4,d5}, [r2]
vmov q1, q4
vswp d2, d3
vbsl q3, q1, q2
vst1.32	{d6,d7}, [r2]
"))

(optimize code (list (list) (list)) 
          #t #:time-limit 36000 #:cores 12)
