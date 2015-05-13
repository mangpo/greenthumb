#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt" "arm-simulator-rosette.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine%))
(send machine set-config (list 4 3 4))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer] [simulator simulator-rosette]))

(define code
(send parser ast-from-string "
mov r1, 21845
movt r1, 21845

mov r2, r0, asr 1
and r2, r1, r2
rsb r2, r2, r0
rev16 r1, r2

mov r2, 13107
movt r2, 13107
and r2, r1, r2
str r2, fp, -12
mov r2, r1, lsr 2
movt r1, 13107
movw r1, 13107
and r2, r1, r2
ldr r1, fp, -12
add r2, r1, r2, lsl 0
mov r1, r2, asr 4
add r2, r1, r2, asr 0
movw r1, 3855
movt r1, 3855
and r2, r1, r2
add r2, r2, r2, asr 16
add r2, r2, r2, lsr 8
and r2, r2, 63
mov r0, r2
"))


(define sketch
(send parser ast-from-string "
mov r1, 21845
movt r1, 21845

and r2, r1, r0, lsr 1
rsb r1, r2, r0



mov r2, 13107
movt r2, 13107
and r2, r1, r2
str r2, fp, -12
mov r2, r1, lsr 2
movt r1, 13107
movw r1, 13107
and r2, r1, r2
ldr r1, fp, -12
add r2, r1, r2, lsl 0
mov r1, r2, asr 4
add r2, r1, r2, asr 0
movw r1, 3855
movt r1, 3855
and r2, r1, r2
add r2, r2, r2, asr 16
add r2, r2, r2, lsr 8
and r2, r2, 63
mov r0, r2
"))

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

#|
(define states
(send validator generate-input-states 8 (vector) (send machine no-assumption) #f
             #:rand-func (lambda () 
                           (if (= (random 2) 0) (random 32) (- (random 32))))))

(for ([state states])
  (send machine display-state state))|#
