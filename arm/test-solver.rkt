#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 5 4 5))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer] [simulator simulator-rosette]))

(define code
(send parser ast-from-string "
        str     r0, [fp, #-16]
        str     r1, [fp, #-20]
        ldr     r2, [fp, #-16]
        ldr     r3, [fp, #-20]
        and     r3, r2, r3
        str     r3, [fp, #-12]
        ldr     r2, [fp, #-16]
        ldr     r3, [fp, #-20]
        eor     r3, r2, r3
        str     r3, [fp, #-8]
        ldr     r3, [fp, #-8]
        mov     r3, r3, asr #1
        str     r3, [fp, #-8]
        ldr     r2, [fp, #-12]
        ldr     r3, [fp, #-8]
        add     r3, r2, r3
        mov     r0, r3
"))


(define sketch
(send parser ast-from-string "
eor r3, r1, r0
mov r4, 1
orn r2, r0, 1
add r4, r2, 1
asr r3, r3, 1
and r1, r1, r0
asr r0, r0, 0
bic r4, r3, 0
mvn r2, r3, asr 1
sub r0, r0, 1
lsl r0, r1, r3
add r1, r1, r4
uxth r4, r1
orr r3, r1, r4
orr r0, r3, r4
bfi r4, r0, 0, 1
and r4, r3, r4
"))
;; 1 hole
; random = 12, 13, 5 | 25, 78
; all-sym = 3, 4, 4 | 2, 3
;; 2 holes
; random = 9184
; random (1) = 2103
; all-sym = 1333

;add r0, r0, r1
;?
;bfc r1, 0, 1
;rsb r1, r1, r0

(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))

(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (constraint machine [reg 0] [mem])))

(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline) 
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

#|
(define t (current-seconds))
(define-values (res cost)
(send solver synthesize-from-sketch
      encoded-code ;; spec
      encoded-sketch ;; sketch = spec in this case
      (constraint machine [reg 1] [mem]) #f #f 36000)
  )
(pretty-display `(time ,(- (current-seconds) t)))|#
