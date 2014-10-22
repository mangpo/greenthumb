#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 4 4))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
	sub	r0, r0, #1
	orr	r0, r0, r0, asr #1
	orr	r0, r0, r0, asr #2
	orr	r0, r0, r0, asr #4
	orr	r0, r0, r0, asr #8
	orr	r0, r0, r0, asr #16
	add	r0, r0, #1
"))

(define sketch
(send parser ast-from-string "
subne r1, r0, 1
clz r2, r1
sub r0, r1, r0
rsbne r0, r0, r0, lsr r2
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

(send printer print-syntax (send printer decode
(send machine clean-code encoded-sketch encoded-code)))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.

#|
(define ex
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0] [mem])))

(when ex 
  (pretty-display "Counterexample:")
  (send machine display-state ex))|#

;; Test solver-based suoptimize function
#|
(define-values (res cost)
(send solver synthesize-from-sketch 
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 0] [mem]) #f))
(send printer print-syntax res)|#

#|
(define live-in
(send solver get-live-in encoded-sketch (constraint machine [reg 0] [mem]) #f))
(send machine display-state live-in)|#

;; This should terminate without an error.
;; If there is error (synthesize fail), that means Rosette might not support 
;; some operations you use arm-simulator-rosette.rkt.
;; Debug this by running the given code in test-simulator.rkt (Section 3).
