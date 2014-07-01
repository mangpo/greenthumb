#lang racket

(require "main.rkt")

(define code 
"
	movsi LR4, -1
	and LR3, LR1, LR0
	xor LR1, LR0, LR4
	and LR2, LR2, LR1
	or LR0, LR2, LR3
")

(optimize code 5 1 (list 0) (list) #f #:dir "test")