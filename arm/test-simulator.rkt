#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         )

(current-bitwidth 32)
(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list  6 3 4)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 4096 1431655765 536870912 4 1048576 524288)
                               (vector 1 131072 536870912) -1 4))

;; Section 1: Concrete program

(define code
(send parser ast-from-string "
and r2, r1, r0, lsr 1
rsb r1, r2, r0
mov r2, 13107
movt r2, 13107
and r2, r1, r2
str r2, fp, -12
mov r2, r1, lsr 2
movt r1, 13107
movw r1, 13107
and r2, r1, r2
ldr r1, fp, -12
add r2, r1, r2, lsl 0
mov r1, r2, asr 4
add r2, r1, r2, asr 0
movw r1, 3855
movt r1, 3855
and r2, r1, r2
add r2, r2, r2, asr 16
add r2, r2, r2, lsr 8
and r2, r2, 63
mov r0, r2
"))

(send printer print-struct code)
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(send printer print-syntax (send printer decode encoded-code))

(define output-state
  (send simulator-racket interpret encoded-code input-state #:dep #t))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
(newline)

;(send simulator-racket performance-cost encoded-code)

;; ;; Section 2: Unknown program
;; ;; ? = one instruction
#|
(define code?
(send parser ast-from-string "
? ?
"))
;; Use validator to encode unknown program instead of printer
(define encoded-code? (send validator encode-sym code?))
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


