#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-parser.rkt" "arm-inst.rkt")

(current-bitwidth 4)
(define parser (new arm-parser%))
(define machine (new arm-machine% [config 4] [bitwidth 4]))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer]
                       [simulator simulator-rosette]))

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

(define encoded-code (send printer encode code))

(send validator adjust-memory-config encoded-code)
