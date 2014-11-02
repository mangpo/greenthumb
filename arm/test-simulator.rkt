#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-solver.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         )

(current-bitwidth 32)
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list  6 4 5)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 7 0 0 0 0 0)
                               (vector 0 0 0 0) -1 5))

;; Section 1: Concrete program
#|
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
"))|#


(define code
(send parser ast-from-string "
	rsb	r1, r0, #0
	and	r1, r1, r0
	add	r4, r1, r0
	eor	r0, r4, r0
	bl	__aeabi_uidiv
	orr	r0, r4, r0, lsr #2
"))

(send printer print-struct code)
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(send printer print-syntax (send printer decode encoded-code))

#|
(send machine display-state
(send solver get-live-in encoded-code 
      (constraint machine [reg 0] [mem]) #f))|#

(define output-state
  (send simulator-racket interpret encoded-code input-state #:dep #t))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
(newline)

(send simulator-racket performance-cost encoded-code)

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


