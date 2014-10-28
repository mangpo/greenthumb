#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-solver.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt")

(current-bitwidth 32)
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 12 16 4)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 1 969976879 0 -172984026 -1 -10 1840791876 501054560 
1518636065 108966344 -102597129 0)
                               (vector -395640832 0 0 0 0 15 0 0 
0 0 0 0 0 0 0 0) 
                               0 4))

;; Section 1: Concrete program
(define code
(send parser ast-from-string "
	ldr	r1, [r4, #4]
	movw	ip, #43691
	movt	ip, 10922
	movw	r3, #65524
	movt	r3, 65535
	smull	lr, ip, ip, r1
	mov	lr, r1, asr #31
	rsb	ip, lr, ip, asr #10
	add	ip, ip, ip, asl #1
	sub	r1, r1, ip, asl #11
	str	r1, [r0, r2, asl #2]
	ldr	lr, [r4, #24]
	ldr	r1, [r5, #40]
	ldr	ip, [r5, #68]
	sub	lr, lr, #1
	ldr	r2, [r4, #36]
	str	r1, [r4, #8]
	str	ip, [r4, #12]
	ldr	r6, [r0, lr, asl #2]
	mov	r0, r2, asl #2
	rsb	r5, r0, r2
	mul	r2, r6, r2
	mov	r5, r5, asl #11
	sub	r5, r5, #12
	rsb	r3, r2, r3
	mla	r3, r5, lr, r3
	mla	r1, ip, r1, r3
	str	r1, [r4, #40]
"))

(send printer print-struct code)
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(send printer print-syntax (send printer decode encoded-code))

;(send machine display-state
;      (last (send solver generate-input-states 2 encoded-code #f)))
#|
(send machine display-state
(send solver get-live-in encoded-code 
      (constraint machine [reg 0] [mem]) #f))|#

(define output-state
  (send simulator-rosette interpret encoded-code input-state #:dep #t))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
(newline)

(send simulator-rosette performance-cost encoded-code)

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

(define (sym-input)
  (define-symbolic* in number?)
  in)
(define input-state-sym (default-state machine sym-input))

(pretty-display "Interpret concrete program with symbolic inputs...")
(define output-state-sym
  (send simulator-rosette interpret encoded-code input-state-sym))
(send machine display-state output-state-sym)


