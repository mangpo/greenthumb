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
	eor	r3, r1, r0
	and	r0, r1, r0
	add	r0, r0, r3, asr #1
"))


(define sketch
(send parser ast-from-string "
and r4, r1, r0
rsb r2, r1, r4, lsl 1
sub r0, r0, r2
sbfx r2, r0, 1, 31
add r0, r2, r4
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

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))

;; Test solver-based suoptimize function
#|
(define t (current-seconds))
(define-values (res cost)
(send solver synthesize-from-sketch 
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 0] [mem]) #f))
(pretty-display `(time ,(- (current-seconds) t)))|#


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
