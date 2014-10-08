#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
sbfx r0, r1, 16, 7
asri r1, r0, 31
rsbi r2, r0, 0
orr r3, r1, r2
"))

(define sketch
(send parser ast-from-string "
sbfx r0, r1, 16, 7
asri r1, r0, 31
rsbi r2, r0, 0
orr r3, r1, r2
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.
(define ex
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0 1 2 3] [mem-all])))

(when ex 
  (pretty-display "Counterexample:")
  (send machine display-state ex))

;; Test solver-based suoptimize function
(send solver superoptimize 
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 0 1 2 3] [mem-all]))

;; This should terminate without an error.
;; If there is error (synthesize fail), that means Rosette might not support 
;; some operations you use arm-simulator-rosette.rkt.
;; Debug this by running the given code in test-simulator.rkt (Section 3).
