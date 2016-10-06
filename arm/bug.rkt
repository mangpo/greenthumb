#lang s-exp rosette

(require "arm-machine.rkt"
         "arm-printer.rkt" "arm-parser.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         "../memory-rosette.rkt" "../validator.rkt"
         )

(current-bitwidth 4)
(define parser (new arm-parser%))
(define machine (new arm-machine% [config 4] [bitwidth 4])) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

(define (func-sym #:min [min #f] #:max [max #f] #:const [const #f])
  (define-symbolic* input number?)
  (when min (assert (>= input min)))
  ;;(when max (assert (<= input max)))
  (if const const input))

(define state (send machine get-state func-sym))

;; Section 1: Concrete program

(define code
(send parser ir-from-string "
	sub	r3, r0, #1
	tst	r3, r0
	movne	r3, #0
	moveq	r3, #1
	cmp	r0, #0
	moveq	r0, #0
	andne	r0, r3, #1
"))

(send printer print-struct code)
(define encoded-code (send printer encode code))
(send printer print-struct encoded-code)

(solve (assert (send simulator-rosette interpret encoded-code state)))