#lang s-exp rosette

(require "$-parser.rkt" "$-printer.rkt" "$-machine.rkt" "../validator.rkt"
         ;;"$-simulator-rosette.rkt" "../memory-racket.rkt"
         ;;"$-simulator-racket.rkt"
         ;;"$-validator.rkt" "$-symbolic.rkt"
         )

;; Phase 0: Set up bitwidth for Rosette
(current-bitwidth ?)

;; Phase A: Test machine, parser, printer
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
;; Phase B: Interpret concrete program with concrete inputs
(pretty-display "Phase B: interpret program using simulator writing in Rosette.")
;; define number of bits used for generating random test inputs
(define test-bit 4)
;; create random input state
(define input-state (send machine get-state (get-rand-func test-bit)))
;; define our own input test, but memory content is random
;; modify program state to match your program state structure
#;(define input-state (progstate (vector ?)
                               (new memory-racket% [get-fresh-val (get-rand-func test-bit)])))
(define simulator-rosette (new $-simulator-rosette% [machine machine]))
(pretty-display `(input ,input-state))
(pretty-display `(output ,(send simulator-rosette interpret encoded-code input-state)))
(newline)

;; Phase C: Interpret concrete program with symbolic inputs
(pretty-display "Phase C: interpret concrete program with symbolic inputs.")
(define input-state-sym (send machine get-state sym-input))
(pretty-display `(input ,input-state-sym))
(pretty-display `(output ,(send simulator-rosette interpret encoded-code input-state-sym)))
(newline)

;; Phase D: Duplicate rosette simulator to racket simulator
(pretty-display "Phase D: interpret program using simulator writing in Racket.")
(define simulator-racket (new $-simulator-racket% [machine machine]))
(send simulator-racket interpret encoded-code input-state)
(newline)

;; Phase E: Interpret symbolic program with symbolic inputs
(pretty-display "Phase E: interpret symbolic program.")
(define validator (new $-validator% [machine machine] [simulator simulator-rosette]))
(define symbolic (new $-symbolic% [machine machine] [printer printer]
                      [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define sym-code (for/vector ([i 2]) (send symbolic gen-sym-inst)))
(send simulator-rosette interpret sym-code input-state-sym)
|#
