#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-parser.rkt")

;;(require rosette/solver/smt/z3)

;;(current-solver (new z3%))

(define parser (new arm-parser%))
(define machine (new arm-machine% [config 5]))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer]
                       [simulator simulator-rosette]))

(define code
(send parser ir-from-string "
	str	r0, [r4, #-40]
	str	r1, [r4, #-44]
	ldr	r3, [r4, #-40]
	uxth	r3, r3
	str	r3, [r4, #-36]
	ldr	r3, [r4, #-40]
	mov	r3, r3, asr #16
	str	r3, [r4, #-32]
	ldr	r3, [r4, #-44]
	uxth	r3, r3
	str	r3, [r4, #-28]
	ldr	r3, [r4, #-44]
	mov	r3, r3, asr #16
	str	r3, [r4, #-24]
	ldr	r3, [r4, #-36]
	ldr	r2, [r4, #-28]
	mul	r3, r2, r3
	str	r3, [r4, #-20]
	ldr	r3, [r4, #-32]
	ldr	r2, [r4, #-28]
	mul	r2, r2, r3
	ldr	r3, [r4, #-20]
	mov	r3, r3, lsr #16
	add	r3, r2, r3
	str	r3, [r4, #-16]
	ldr	r3, [r4, #-16]
	uxth	r3, r3
	str	r3, [r4, #-12]
	ldr	r3, [r4, #-16]
	mov	r3, r3, asr #16
	str	r3, [r4, #-8]
	ldr	r3, [r4, #-24]
	ldr	r2, [r4, #-36]
	mul	r2, r2, r3
	ldr	r3, [r4, #-12]
	add	r3, r2, r3
	str	r3, [r4, #-12]
	ldr	r3, [r4, #-32]
	ldr	r2, [r4, #-24]
	mul	r2, r2, r3
	ldr	r3, [r4, #-8]
	add	r2, r2, r3
	ldr	r3, [r4, #-12]
	mov	r3, r3, asr #16
	add	r3, r2, r3
	mov	r0, r3
"))


(define sketch
(send parser ir-from-string "
smmul r0, r0, r1
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode  sketch))

(send validator adjust-memory-config encoded-code)
(define t1 (current-seconds))
(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (send printer encode-live '(0))))
(define t2 (current-seconds))

(newline)
(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
(pretty-display `(time ,(- t2 t1)))
