#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-parser.rkt" "arm-inst.rkt")

;;(require rosette/solver/smt/z3)

;;(current-solver (new z3%))

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


(define sketch
(send parser ir-from-string "
clz r3, r0
sub r3, r3, r0, lsl r3
lsr r0, r3, 3
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
