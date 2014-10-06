#lang s-exp rosette

(require "main.rkt" "arm-parser.rkt")

(define parser (new arm-parser%))

;; Parse from string
(define code
(send parser ast-from-string "
	movsi LR1, 0
	sub LR2, LR1, LR0
	or LR3, LR0, LR2
	srai LR0, LR3, 31
"))

;; Parse from file
(define code2 (send parser ast-from-file "programs/p13_sign.s"))


(optimize-solver code2 
                 (list 0)   ;; a list of live registers' IDs
                 (list 4 1) ;; (list num-regs memory-size) the smaller the better
                 #:size 4   ;; length of the output program
                 )
