#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 5 5))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
	eor	r3, r1, r0
	and	r0, r1, r0
	add	r0, r0, r3, asr #1
"))

(define sketch
(send parser ast-from-string "
eor r3, r1, r0
and r1, r1, r0
str r1, fp, -12
str r3, fp, -8
ldr r3, fp, -8
mov r3, r3, asr 1
str r3, fp, -8
ldr r2, fp, -12
ldr r3, fp, -8
add r3, r2, r3
mov r0, r3

"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

;(send printer print-syntax (send printer decode
;(send machine clean-code encoded-sketch encoded-code)))

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

#|
(define live-in
(send solver get-live-in encoded-sketch (constraint machine [reg 0] [mem]) #f))
(send machine display-state live-in)|#

;; This should terminate without an error.
;; If there is error (synthesize fail), that means Rosette might not support 
;; some operations you use arm-simulator-rosette.rkt.
;; Debug this by running the given code in test-simulator.rkt (Section 3).
