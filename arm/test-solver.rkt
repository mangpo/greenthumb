#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 4 5))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]
                    [parser parser] [syn-mode `partial1]))

(define code
(send parser ast-from-string "
	str	r0, [fp, #-8]
	ldr	r3, [fp, #-8]
	sub	r3, r3, #1
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #1
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #2
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #4
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #8
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r3, r3, asr #16
	ldr	r2, [fp, #-8]
	orr	r3, r2, r3
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	add	r3, r3, #1
	str	r3, [fp, #-8]
	ldr	r3, [fp, #-8]
	mov	r0, r3
"))


(define sketch
(send parser ast-from-string "
sub r0, r0, 1
mvn r1, 0
? ?
"))

;; no hi, ls 
;; solver 50, 63, 28
;; p13_o0 stoch 7 12 8

;; with hi, ls
;; cmpne & better performance model
;; solver 113, 70, 15
;; p13_o0 8(4), 11(4)
;; max 36(3), 22(3), 13(3)

;; merge (misal)
;; p1 54(2), 18(2), 90(3)
;; p13 55(3), 40(4), 67(3), 35(4)
;; max 46(3), 99(3)

;; merge (half)
;; solver 9, > 180, 162
;; max 54(3), 59(3)

;; 1 input
;; 21, 103, 17

;; add smmul
;; 10, 285, 111 (timeout 120)

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))
;(send printer print-syntax (send printer decode
;(send machine clean-code encoded-sketch encoded-code)))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.


(define ex
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0] [mem])))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))

;; Test solver-based suoptimize function

(define t (current-seconds))
(define-values (res cost)
(send solver synthesize-from-sketch 
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 0] [mem]) #f))
(pretty-display `(time ,(- (current-seconds) t)))


#|
(define res
  (send solver superoptimize 
        encoded-code 
        (constraint machine [reg 0] [mem]) "./foo" 3600 #f))|#

#|
(define live-in
(send solver get-live-in encoded-code (constraint machine [reg 0] [mem]) #f))
(send machine display-state live-in)|#

;; This should terminate without an error.
;; If there is error (synthesize fail), that means Rosette might not support 
;; some operations you use arm-simulator-rosette.rkt.
;; Debug this by running the given code in test-simulator.rkt (Section 3).
