#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-solver.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         )

(current-bitwidth 32)
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list  6 6 6)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 0 0 0 0 0 0 )
                               (vector 0 0 0 0 0 0 0) -1 6))

;; Section 1: Concrete program

(define code
(send parser ast-from-string "
	cmp	r1, #-2147483648
	add	r3, r1, #7
	mov	r5, r1, asr #31
	movcc	r3, r1
	cmp	r2, #0
	mov	r2, r3, asr #3
	mov	r3, r5, lsr #29
	mov	r4, #1
	add	r1, r1, r3
	and	r1, r1, #7
	rsb	r3, r3, r1
	orrne	r1, r0, r4, asl r3
	biceq	r1, r0, r4, asl r3
	uxtbne	r1, r1
"))

(send printer print-struct code)
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(send printer print-syntax (send printer decode encoded-code))

#|
(define output-state
  (send simulator-racket interpret encoded-code input-state #:dep #t))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
(newline)

(send simulator-racket performance-cost encoded-code)|#

(send printer get-constants encoded-code)

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
#|
(define (sym-input)
  (define-symbolic* in number?)
  in)
(define input-state-sym (default-state machine sym-input))

(pretty-display "Interpret concrete program with symbolic inputs...")
(define output-state-sym
  (send simulator-rosette interpret encoded-code input-state-sym))
(send machine display-state output-state-sym)|#


