#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 6 6 6))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]
                    [parser parser] [syn-mode `partial1]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

(define code
(send parser ast-from-string "
	str     r0, [fp, #-24]
	ldr     r3, [fp, #-24]
	sub     r3, r3, #1
	str     r3, [fp, #-16]
	ldr     r2, [fp, #-16]
	ldr     r3, [fp, #-24]
	and     r3, r2, r3
	str     r3, [fp, #-16]
	ldr     r3, [fp, #-16]
	cmp     r3, #0
	movne	r3, #0
        moveq	r3, #1
	str     r3, [fp, #-12]
	ldr     r3, [fp, #-12]
        ldr	r2, [fp, #-24]
        cmp     r2, #0
        moveq   r2, #0
        andne	r2, r3, #1
        mov     r0, r2
"))


(define sketch
(send parser ast-from-string "
	sub	r3, r0, #1
	tst	r3, r0
	movne	r3, #0
	moveq	r3, #1
	cmp	r0, #0
	moveq	r0, #0
	andne	r0, r3, #1
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
#|
;; Counterexample:
(define input-state (progstate (vector 242087795 -1555402324 0 0 0 0)
                               (vector 0 0 0 0) -1 5))

(pretty-display "Output 1")
(send machine display-state (send simulator-rosette interpret encoded-code input-state))
(newline)

(pretty-display "Output 2")
(send machine display-state (send simulator-rosette interpret encoded-sketch input-state))
|#
