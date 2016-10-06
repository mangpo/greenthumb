#lang s-exp rosette

(require "arm-machine.rkt"
         "arm-printer.rkt" "arm-parser.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-simulator-racket.rkt"
         "../memory-rosette.rkt"
         )

(current-bitwidth 4)
(define parser (new arm-parser%))
(define machine (new arm-machine% [config 4] [bitwidth 4])) ;; argument = (list num-regs memory)
(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

(define (func-rand #:min [min #f] #:max [max #f] #:const [const #f])
  (if const const (random 10)))

(define (func-sym #:min [min #f] #:max [max #f] #:const [const #f])
  (define-symbolic* input number?)
  (if const const input))

;; Input machine state
(define input-state (progstate (vector -3 0 0 0 0
                                       0 0 0 0 0
                                       0 0)
                               (new memory-rosette% [get-fresh-val func-sym]) -1))

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
;;(send printer print-syntax (send printer decode encoded-code))

(define output-state
  (send simulator-rosette interpret encoded-code input-state))
(pretty-display "Output from simulator in rosette.")
(send machine display-state output-state)

;; (pretty-display
;;  (send enum abstract 
;;        (send machine progstate->vector output-state) (list 0 2 3 4) 8))
;; (newline)

;;(send simulator-racket performance-cost encoded-code)


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


