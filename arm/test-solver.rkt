#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 3 1 0))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]
                    [parser parser] [syn-mode `partial1]))

#|
	ldr	r1, [r4, #4]
	movw	ip, #43691
	movt	ip, 10922
	movw	r3, #65524
	movt	r3, 65535
	smull	lr, ip, ip, r1
	mov	lr, r1, asr #31
	rsb	ip, lr, ip, asr #10
	add	ip, ip, ip, asl #1
	sub	r1, r1, ip, asl #11
	str	r1, [r0, r2, asl #2]
	ldr	lr, [r4, #24]
	ldr	r1, [r5, #40]
	ldr	ip, [r5, #68]
	sub	lr, lr, #1
	ldr	r2, [r4, #36]
	str	r1, [r4, #8]
	str	ip, [r4, #12]
	ldr	r6, [r0, lr, asl #2]
	mov	r0, r2, asl #2
	rsb	r5, r0, r2
	mul	r2, r6, r2
	mov	r5, r5, asl #11
	sub	r5, r5, #12
	rsb	r3, r2, r3
	mla	r3, r5, lr, r3
	mla	r1, ip, r1, r3
	str	r1, [r4, #40]
|#

(define code
(send parser ast-from-string "
        add     r1, r1, r2
        mov     r2, r1, lsr #31
        add     r1, r1, r2
        and     r1, r1, #1
        rsb     r2, r2, r1
"))

(define sketch
(send parser ast-from-string "
        add     r1, r1, r2
? ?
        rsb     r2, r2, r1
")) 
;; no div, no inputs 46, 44
;; choice div, no inputs 41
;; support div, no inputs 119, 115

;; no div, inputs 61 105
;; no div, 1 inputs (0) 76
;; no div, 1 inputs (random) 41, 43
;; no div, 2 inputs (random) 3, 23, 11, 51
;; adjust cost function
;; no div, 3 inputs (random) 25, 31

;; support div, 2 inputs (random) 89

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

;(send printer print-syntax (send printer decode
;(send machine clean-code encoded-sketch encoded-code)))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.

#|
(define ex
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0 1] [mem])))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))|#

;; Test solver-based suoptimize function

(define t (current-seconds))
(define-values (res cost)
(send solver synthesize-from-sketch 
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 2] [mem-all]) #f))
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
