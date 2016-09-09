#lang s-exp rosette

(require "llvm-demo-parser.rkt" "llvm-demo-printer.rkt" "llvm-demo-machine.rkt"
         "llvm-demo-simulator-rosette.rkt"
         "llvm-demo-simulator-racket.rkt")


;; Phase 0: Set up bitwidth for Rosette
(current-bitwidth 32)

;; Phase A: Test machine, parser, printer (step 1 & 2)
(pretty-display "Phase A: test machine, parser, and printer.")
(define parser (new llvm-demo-parser%))
(define machine (new llvm-demo-machine% [config 4]))
(define printer (new llvm-demo-printer% [machine machine]))

;; clear 3 lowest bits.
(define code
(send parser ir-from-string "
%1 = lshr i32 %in, 3
%out = shl nuw i32 %1, 3
"))

(send printer print-struct code)
(send printer print-syntax code)

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(newline)

;; Phase B: Interpret concrete program with concrete inputs (step 3)
(pretty-display "Phase B: interpret program using simulator writing in Rosette.")
(define input-state (vector 1 22 3 4))
(define simulator-rosette (new llvm-demo-simulator-rosette% [machine machine]))
(send simulator-rosette interpret encoded-code input-state)
(newline)


;; Phase C: Interpret concrete program with symbolic inputs
(pretty-display "Phase C: interpret concrete program with symbolic inputs.")
(define (sym-input)
  (define-symbolic* in number?)
  in)

(define input-state-sym (send machine get-state sym-input))
(send simulator-rosette interpret encoded-code input-state-sym)
(newline)

;; Phase D: Duplicate rosette simulator to racket simulator
(pretty-display "Phase D: interpret program using simulator writing in Racket.")
(define simulator-racket (new llvm-demo-simulator-racket% [machine machine]))
(send simulator-racket interpret encoded-code input-state)
