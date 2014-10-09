#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-solver.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt")

(configure [bitwidth 32])
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 4)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 0 #x74377633 17 7 55)
                               (vector 111 222 333 444)))

;; Section 1: Concrete program
(define code
(send parser ast-from-string "
	rsb	r5, r0, #0
	and	r5, r5, r0
	mov	r4, r0
	eor	r0, r5, r0
	add	r4, r4, r5
	mov	r1, r5
	mov	r0, r0, asr #2
	mov	r1, r5
	mov	r0, r0, asr #2
	bl	__aeabi_idiv
"))
(send printer print-struct code)
;(define encoded-code (send printer encode code))

#|
(define output-state
  (send simulator-rosette interpret encoded-code input-state #:dep #f))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
(newline)|#

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
