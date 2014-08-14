#lang racket

(require "../main.rkt" "parser.rkt")

(define code
(ast-from-string "
vld1 {d9}, [r0]
vld1 {d9}, [r0]
VMLAL.S16 q0, d3, d2[0]
"))

(optimize code (list (list 0 1) (list)) #t #:time-limit 60)