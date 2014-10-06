#lang racket

(require "main.rkt" "arm-parser.rkt")

(define code (send (new arm-parser%) ast-from-string "
xor LR2, LR1, LR0
sgeu LR3, LR0, LR1
movsi LR4, 0
skpbs LR3, LR4
addi LR2, LR4, 0
xor LR0, LR2, LR1"))
                   

(arm-generate-inputs code (list 5 1) "data")

;; (require "../arm-machine.rkt" "../arm-printer.rkt" "../arm-parser.rkt"
;;          "../arm-solver.rkt" "../arm-stochastic.rkt"
;;          "../arm-simulator-rosette.rkt"
;;          "../../fitness-learner.rkt")

;; (define parser (new arm-parser%))
;; (define machine (new arm-machine%))
;; (send machine set-config (list 5 1)) ;; argument = (list num-regs memory)
;; (define printer (new arm-printer% [machine machine]))
;; (define solver (new arm-solver% [machine machine] [printer printer]))

;; (define code
;; (send parser ast-from-string "
;; xor LR2, LR1, LR0
;; sgeu LR3, LR0, LR1
;; movsi LR4, 0
;; skpbs LR3, LR4
;; addi LR2, LR4, 0
;; xor LR0, LR2, LR1
;; "))

;; (define encoded-code (send printer encode code))

;; (generate-inputs encoded-code #f "data" machine printer solver)