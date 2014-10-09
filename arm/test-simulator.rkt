#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-solver.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt")

(configure [bitwidth 32])
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 6 4)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 9 6 12 -49 0 0)
                               (vector 111 222 333 444)))

;; Section 1: Concrete program
(define code
(send parser ast-from-string "
	movw	r1, #21845
	bfi	r1, r1, #16, #16
	and	r1, r1, r0, asr #1
	movw	r2, #13107
	rsb	r1, r1, r0
	movt	r2, 13107
	movw	r3, #13107
	bfi	r3, r3, #16, #16
	and	r2, r1, r2
	and	r1, r3, r1, asr #2
	add	r2, r2, r1
	movw	r3, #3855
	movt	r3, 3855
	add	r2, r2, r2, asr #4
	and	r3, r2, r3
	add	r0, r3, r3, lsr #8
	add	r0, r0, r0, lsr #16
"))
(send printer print-struct code)
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)


(define output-state
  (send simulator-rosette interpret encoded-code input-state #:dep #f))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
(newline)

;; ;; Section 2: Unknown program
;; ;; ? = one instruction
#|
(define code?
(send parser ast-from-string "
? ?
"))
;; Use solver to encode unknown program instead of printer
(define encoded-code? (send solver encode-sym code?))
(pretty-display "Interpret unknown program using simulator written in rosette...")
(define output-state?
  (send simulator-rosette interpret encoded-code? input-state))
(send machine display-state output-state?)
(newline)|#

;; ;; Section 3: Symbolic inputs
;; ;; Concrete program with symbolic inputs
#|(define (sym-input)
  (define-symbolic* in number?)
  in)
(define input-state-sym (default-state machine sym-input))

(pretty-display "Interpret concrete program with symbolic inputs...")
(define output-state-sym
  (send simulator-rosette interpret encoded-code input-state-sym))
(send machine display-state output-state-sym)|#
