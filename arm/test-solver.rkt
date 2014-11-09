#lang s-exp rosette

(require "arm-solver.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 4 4))
(define printer (new arm-printer% [machine machine]))
(define solver (new arm-solver% [machine machine] [printer printer]
                    [parser parser] [syn-mode `partial1]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))

(define code
(send parser ast-from-string "
        str     r0, [fp, #-16]
        ldr     r3, [fp, #-16]
        mov     r3, r3, asr #31
        str     r3, [fp, #-12]
        ldr     r3, [fp, #-16]
        rsb     r3, r3, #0
        str     r3, [fp, #-8]
        ldr     r3, [fp, #-8]
        mov     r3, r3, asr #31
        str     r3, [fp, #-8]
        ldr     r2, [fp, #-12]
        ldr     r3, [fp, #-8]
        orr     r3, r2, r3
        mov     r0, r3
"))


(define sketch
(send parser ast-from-string "
cmp r0, r1
orrne r2, r1, r1
cmp r0, r3
movne r0, r3
orreq r0, r2, 0
")) 

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))

;(send printer print-syntax (send printer decode
;(send machine clean-code encoded-sketch encoded-code)))

;; Return counterexample if code and sketch are different.
;; Otherwise, return #f.

#|
(define ex 
  (send solver counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0] [mem])))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline) |#
#|
;; Counterexample:
(define input-state (progstate (vector 242087795 -1555402324 0 0 0 0)
                               (vector 0 0 0 0) -1 5))

(pretty-display "Output 1")
(send machine display-state (send simulator-rosette interpret encoded-code input-state))
(newline)

(pretty-display "Output 2")
(send machine display-state (send simulator-rosette interpret encoded-sketch input-state))
|#

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
