#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 1 0))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]
                    [parser parser] [syn-mode `partial1]))

(define code
(send parser ast-from-string "
	smull r1, r0, r1, r0
"))

(define sketch
(send parser ast-from-string "
	?
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

;(define encoded-code (send printer encode code))
;(define encoded-sketch (send solver encode-sym sketch))
(define encoded-code
  (vector
   (arm-inst 21 '#(3 43691) #f #f -1)
   (arm-inst 22 '#(3 10922) #f #f -1)
   (arm-inst 21 '#(1 65524) #f #f -1)
   (arm-inst 22 '#(1 65535) #f #f -1)
   (arm-inst 36 '#(2 3 3 0) #f #f -1)
   (arm-inst 17 '#(2 0) 4 31 -1)
   (arm-inst 3 '#(3 2 3) 4 10 -1)
   (arm-inst 1 '#(3 3 3) 5 1 -1)
   (arm-inst 2 '#(0 0 3) 5 11 -1)))

(define encoded-sketch
  (vector
(arm-inst 22 '#(1 65535) #f #f -1)
(arm-inst 22 '#(3 10922) #f #f -1)
(arm-inst 21 '#(3 43691) #f #f -1)
(arm-inst 36 '#(2 3 3 0) #f #f -1)
(arm-inst 21 '#(1 65524) #f #f -1)
(arm-inst 17 '#(2 0) 4 31 -1)
(arm-inst 3 '#(3 2 3) 6 10 -1)
(arm-inst 1 '#(3 3 3) 5 1 -1)
(arm-inst 2 '#(0 0 3) 5 11 -1)
   ))

;(send printer print-syntax (send printer decode
;(send machine clean-code encoded-sketch encoded-code)))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.


(define ex
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0 1] [mem])))

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
