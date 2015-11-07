#lang s-exp rosette

(require "arm-machine.rkt" "arm-printer.rkt" "arm-parser.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         )

(current-bitwidth 4)
(define parser (new arm-parser%))
(define machine (new arm-machine% [bit 32]))
(send machine set-config (list 4 1 2)) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

;; Input machine state
(define input-state (progstate (vector 0 999 0 0)
                               (vector 0) -1 2))

;; Section 1: Concrete program

(define code
(send parser ast-from-string "
mvn r1, 11
"))

(send printer print-struct code)
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(send printer print-syntax (send printer decode encoded-code))

(define output-state
  (send simulator-racket interpret encoded-code input-state))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)
;; (pretty-display
;;  (send enum abstract 
;;        (send machine progstate->vector output-state) (list 0 2 3 4) 8))
;; (newline)

(send simulator-racket performance-cost encoded-code)

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


