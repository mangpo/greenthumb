#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 6 4 5))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]
                    [parser parser] [syn-mode `partial1]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

(define code
(send parser ast-from-string "
	uxth	r5, r0
	mov	r3, r0, asr #16
	uxth	r4, r1
	mov	r1, r1, asr #16
	mul	r2, r4, r5
	mul	r4, r4, r3
	mul	r0, r1, r5
	add	r2, r4, r2, asr #16
	uxtah	r0, r0, r2
	mov	r2, r2, asr #16
	add	r0, r2, r0, asr #16
	mla	r0, r1, r3, r0
"))


(define sketch
(send parser ast-from-string "
smmul r0, r0, r1
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

(define ex 
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0] [mem])))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)

;; Counterexample:
(define input-state (progstate (vector 242087795 -1555402324 0 0 0 0)
                               (vector 0 0 0 0) -1 5))

(pretty-display "Output 1")
(send machine display-state (send simulator-rosette interpret encoded-code input-state))
(newline)

(pretty-display "Output 2")
(send machine display-state (send simulator-rosette interpret encoded-sketch input-state))
