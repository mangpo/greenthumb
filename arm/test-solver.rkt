#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-simulator-rosette.rkt" 
         "arm-parser.rkt" "arm-inst.rkt")

(require rosette/solver/smt/z3)

(current-solver (new z3%))

(define parser (new arm-parser%))
(define machine (new arm-machine% [config 12]))
(define printer (new arm-printer% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [printer printer]
                       [simulator simulator-rosette]))

(define code
(send parser ir-from-string "
uxth r5, r0
mov r3, r0, asr 16
uxth r4, r1
mov r1, r1, asr 16
mul r2, r5, r4
mul r4, r3, r4
mul r5, r5, r1
add r2, r4, r2, lsr 16
mov r0, r2, asr 16
uxtah r2, r5, r2
mla r0, r1, r3, r0
add r0, r0, r2, asr 16
"))


(define sketch
(send parser ir-from-string "
add r4, r0, r0
nop
lsl r3, r1, r0
rsb r4, r3, r0, ror 1
eor r4, r4, r0, asr 16
movt r2, 0
and r5, r0, r3
asr r4, r1, r5
smmul r0, r4, r0
eor r2, r5, r0
tst r2, 1
rsb r5, r2, r1, ror 0
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode  sketch))

(define t1 (current-seconds))
(define ex 
  (send validator counterexample encoded-code encoded-sketch 
        (send printer encode-live '(0))))
(define t2 (current-seconds))

(send validator adjust-memory-config encoded-code)
(pretty-display "Counterexample:")
(if ex 
  (send machine display-state ex)
  (pretty-display "No"))
(newline)
(pretty-display `(time ,(- t2 t1)))

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
