#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 7 8))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
	sub	r3, r0, #1
	orr	r3, r3, r0
	add	r3, r3, #1
	and	r0, r3, r0
"))

(define sketch
(send parser ast-from-string "
orr r3, r0, r0, lsl 1
sub r2, r0, r3
and r0, r0, r2

"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.
(define ex
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0] [mem])))

(when ex 
  (pretty-display "Counterexample:")
  (send machine display-state ex))

;; Test solver-based suoptimize function
#|
(define-values (res cost)
(send solver synthesize-from-sketch 
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 0] [mem]) #f))
(send printer print-syntax res)|#

;; This should terminate without an error.
;; If there is error (synthesize fail), that means Rosette might not support 
;; some operations you use arm-simulator-rosette.rkt.
;; Debug this by running the given code in test-simulator.rkt (Section 3).
