#lang racket                            ;

(require "main.rkt")

;; md5
;; (define code 
;; "
;; 	movsi LR4, -1
;; 	and LR3, LR1, LR0
;; 	xor LR1, LR0, LR4
;; 	and LR2, LR2, LR1
;; 	or LR0, LR2, LR3
;; ")

;; bset
;; (define code
;; "	movsi LR2, 1
;; 	sll LR3, LR2, LR1
;; 	or LR0, LR3, LR0
;; ")

;; p13_sign
(define code
"	movsi LR1, 0
	sub LR2, LR1, LR0
	or LR3, LR0, LR2
	srai LR0, LR3, 31")

;; p16_max
;; (define code
;; "xor LR2, LR0, LR1
;; sgeu LR3, LR0, LR1
;; movsi LR4, 0
;; andi LR3, LR3, 1
;; sub LR3, LR4, LR3
;; and LR0, LR2, LR3
;; xor LR0, LR0, LR1")

;; p18_power
;; (define code "
;; 	addi LR1, LR0, -1
;; 	and LR2, LR0, LR1
;; 	snei LR4, LR0, 0
;; 	andi LR4, LR4, 1
;; 	snei LR3, LR2, 0
;; 	andi LR3, LR3, 1
;; 	seqi LR3, LR3, 0
;; 	andi LR3, LR3, 1
;; 	and LR0, LR3, LR4")

(optimize code 5 1 (list 0) (list) #t #:dir "output" #:cores 12)
