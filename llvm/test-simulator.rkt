#lang s-exp rosette

(require "llvm-parser.rkt" "llvm-printer.rkt" "llvm-machine.rkt"
         "llvm-simulator-rosette.rkt"
         "llvm-simulator-racket.rkt"
         "../memory-racket.rkt" "../validator.rkt")

;; Phase 0: Set up bitwidth for Rosette
(current-bitwidth 32)

;; Phase A: Test machine, parser, printer (step 1 & 2)
(pretty-display "Phase A: test machine, parser, and printer.")
(define parser (new llvm-parser%))
(define machine (new llvm-machine% [config (cons 4 1)]))
(define printer (new llvm-printer% [machine machine]))

;; clear 3 lowest bits.
(define code
(send parser ir-from-string "
%1 = load i32, i32* %2
%1 = udiv i32 %1, 2
store i32 %1, i32* %2
"))

;%1 = load i32, i32* %2
;%1 = add i32 %1, 1
;store i32 %1, i32* %2

;%1 = add <4 x i32> %1, %2

#;(define-values (compressed-code compressed-live-out xxx config)
  (send printer compress-state-space code '#((%out) () #t)))
(send printer print-struct code)
(send printer print-syntax code)

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(newline)

;; Phase B: Interpret concrete program with concrete inputs (step 3)
(pretty-display "Phase B: interpret program using simulator writing in Rosette.")
(define input-state (vector (vector 111 222 333 444)
                            (for/vector ([i 4]) (for/vector ([j 4]) 1))
                            (new memory-racket% [init (make-hash '((222 . 2222)))])
                            ))
(define simulator-rosette (new llvm-simulator-rosette% [machine machine]))
(define out (send simulator-rosette interpret encoded-code input-state))
(send machine display-state out)
(newline)

;; Phase C: Interpret concrete program with symbolic inputs
(pretty-display "Phase C: interpret concrete program with symbolic inputs.")

(define input-state-sym (send machine get-state sym-input))
(define out-sym (send simulator-rosette interpret encoded-code input-state-sym))
(newline)
(send machine display-state out-sym)

#|
;; Phase D: Duplicate rosette simulator to racket simulator
(pretty-display "Phase D: interpret program using simulator writing in Racket.")
(define simulator-racket (new llvm-simulator-racket% [machine machine]))
(send simulator-racket interpret encoded-code input-state)

|#
