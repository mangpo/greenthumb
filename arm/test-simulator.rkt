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
(define input-state (progstate (vector 9 10641 15 -49 6 0 0 0 0 5 5)
                               (vector 0 0 0 0 0 0 0 0 0 0)))

;; Section 1: Concrete program
(define code
(send parser ast-from-string "
        eor     r0, r0, r0, asr #1
        movw    r3, #4369
        movt    r3, 4369
        eor     r0, r0, r0, asr #2
        and     r3, r0, r3
        add     r3, r3, r3, asl #4
        add     r3, r3, r3, asl #8
        add     r0, r3, r3, asl #16

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
  (send simulator-rosette interpret encoded-code input-state #:dep #t))
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


