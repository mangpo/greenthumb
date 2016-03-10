#lang s-exp rosette

(require "$-parser.rkt" "$-printer.rkt" "$-machine.rkt"
         ;;"$-simulator-rosette.rkt"
         ;;"$-simulator-racket.rkt"
         )

;; Phase 0: Set up bitwidth for Rosette
(current-bitwidth ?)

;; Phase A: Test machine, parser, printer (step 1 & 2)
(pretty-display "Phase A: test machine, parser, and printer.")
(define parser (new $-parser%))
(define machine (new $-machine% [config ?]))
(define printer (new $-printer% [machine machine]))

(define code
(send parser ir-from-string "
code here
"))

(pretty-display ">>> Source")
(send printer print-syntax code)

(pretty-display ">>> String-IR")
(send printer print-struct code)

(pretty-display ">>> Encoded-IR")
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(newline)

#|
;; Phase B: Interpret concrete program with concrete inputs (step 3)
(pretty-display "Phase B: interpret program using simulator writing in Rosette.")
(define input-state ?)
(define simulator-rosette (new $-simulator-rosette% [machine machine]))
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
(define simulator-racket (new $-simulator-racket% [machine machine]))
(send simulator-racket interpret encoded-code input-state)
|#
