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
sbfx r0, r1, 16, 7
asri r1, r0, 31
rsbi r2, r0, 0
orr r3, r1, r2
"))
(define encoded-code (send printer encode code))

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
(define (sym-input)
  (define-symbolic* in number?)
  in)
(define input-state-sym (default-state machine sym-input))

(pretty-display "Interpret concrete program with symbolic inputs...")
(define output-state-sym
  (send simulator-rosette interpret encoded-code input-state-sym))
(send machine display-state output-state-sym)
