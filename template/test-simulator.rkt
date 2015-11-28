#lang s-exp rosette

(require "$-parser.rkt" "$-printer.rkt" "$-machine.rkt"
         "$-simulator-rosette.rkt"
         "$-simulator-racket.rkt")

;; Step 0: set up bitwidth for Rosette
(current-bitwidth ?)

;; Step 1: Test parser and printer
(pretty-display "Step 1: test parser and printer.")
(define parser (new $-parser%))
(define machine (new $-machine% [config ?]))
(define printer (new $-printer% [machine machine]))

(define code
(send parser ir-from-string "
code here
"))

(send printer print-struct code)
(send printer print-syntax code)

(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)
(newline)

#|
;; Step 2: Test concrete simulator
(pretty-display "Step 2: interpret program using simulator writing in Rosette.")
(define input-state ?)
(define simulator-rosette (new $-simulator-rosette% [machine machine]))
(send simulator-rosette interpret encoded-code input-state)
(newline)

;; Step 3: interpret concrete program with symbolic inputs
(pretty-display "Step 5: interpret concrete program with symbolic inputs.")
(define (sym-input)
  (define-symbolic* in number?)
  in)

(define input-state-sym (send machine get-state sym-input))
(send simulator-rosette interpret encoded-code input-state-sym)
(newline)

;; Step 4: duplicate rosette simulator to racket simulator
(pretty-display "Step 6: interpret program using simulator writing in Racket.")
(define simulator-racket (new $-simulator-racket% [machine machine]))
(send simulator-racket interpret encoded-code input-state)
|#
