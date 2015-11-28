#lang racket

(require "main.rkt" "neon-parser.rkt")

(define parser (new neon-parser%))

(define code
(send parser ir-from-string "
vorr q3, q0, q0
vld1 {d4,d5}, [r0]
vorr d2, d9, d9
vorr d3, d8, d8
vbsl q3, q1, q2
vst1.32	{d6,d7}, [r0]
"))

(optimize code (list (list 0 1) (list)) 
          #t #:time-limit 72000 #:cores 8 #:dir "output4")
