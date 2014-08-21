#lang s-exp rosette

(require "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt" 
         "neon-simulator-rosette.rkt" "neon-simulator-racket.rkt")


(define parser (new neon-parser%))
(define machine (new neon-machine%))
(define printer (new neon-printer% [machine machine]))
(define simulator (new neon-simulator-rosette% [machine machine]))
(define simulator-racket (new neon-simulator-racket% [machine machine]))

(define code
(send parser ast-from-string "
vld1 {d9}, [r0]
vld1 {d9}, [r0]
VMLAL.S16 q0, d3, d2[0] 
"))

(define encoded-code (send printer encode code))

(send printer print-struct encoded-code)

(define (sym-input)
  (define-symbolic* input number?)
  input)

(define (input) 0)

(define (loop)
  (pretty-display `(mem ,(quotient (current-memory-use) 1000)))

  ;; Rosette
  (let ([state (default-state machine sym-input)])
    (send simulator interpret encoded-code state))

  ;; Racket
  ;; (let ([state (default-state machine input)])
  ;;   (send simulator-racket interpret encoded-code state))
  (clear-asserts)
  (clear-terms!)

  (loop))
(loop)