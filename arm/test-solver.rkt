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
	str	r0, [fp, #-24]
	ldr	r3, [fp, #-24]
	rsb	r3, r3, #0
	mov	r2, r3
	ldr	r3, [fp, #-24]
	and	r3, r2, r3
	str	r3, [fp, #-20]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-20]
	add	r3, r2, r3
	str	r3, [fp, #-16]
	ldr	r2, [fp, #-24]
	ldr	r3, [fp, #-16]
	eor	r3, r2, r3
	str	r3, [fp, #-12]
	ldr	r0, [fp, #-12]
	ldr	r1, [fp, #-20]
	bl	__aeabi_uidiv
	mov	r3, r0
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, lsr #2
	str	r3, [fp, #-8]
	ldr	r2, [fp, #-8]
	ldr	r3, [fp, #-16]
	orr	r3, r2, r3
	mov	r0, r3
"))


(define sketch
(send parser ast-from-string "
	rsb	r1, r0, #0
	and	r1, r1, r0
	add	r4, r1, r0
	eor	r0, r4, r0
	bl	__aeabi_uidiv
	orr	r0, r4, r0, lsr #2
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