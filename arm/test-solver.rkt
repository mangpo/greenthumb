#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 1 0))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]
                    [parser parser]
                    [syn-mode `partial2]))

(define code
(send parser ast-from-string "
	orr	r3, r1, r0
	eor	r0, r1, r0
	sub	r0, r3, r0, asr #1
"))


(define sketch
(send parser ast-from-string "
rsb r0, r3, 9
sbfx r2, r1, 3, 12
mov r3, 1024
eor r2, r2, 8
sbfx r3, r1, 1, 31
add r4, r3, r0, asr 1
add r3, r1, r3, asr 23
orr r3, r0, r1
rev r0, r3
and r3, r3, 1
orr r4, r3, 3
add r0, r4, r3
mvn r0, r2

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


(define ex
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0] [mem])))

(when ex 
  (pretty-display "Counterexample:")
  (send machine display-state ex))

;; Test solver-based suoptimize function
#|
(define t (current-seconds))
(define-values (res cost)
(send solver synthesize-from-sketch 
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 0] [mem]) #f))
(pretty-display `(time ,(- (current-seconds) t)))
|#

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
