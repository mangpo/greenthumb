#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "arm-ast.rkt")

(define parser (new arm-parser%))
(define machine (new arm-machine% [bit 4]))
(send machine set-config (list 6 12 12))
(define printer (new arm-printer% [machine machine]))
(define validator (new arm-validator% [machine machine]))

(define code
(send parser ast-from-string "
	uxth	r5, r0
	mov	r3, r0, asr #2
	uxth	r4, r1
	mov	r1, r1, asr #2
	mul	r2, r5, r4
	mul	r4, r3, r4
	mul	r5, r5, r1
	add	r2, r4, r2, lsr #2
	mov	r0, r2, asr #2
	uxtah	r2, r5, r2
	mla	r0, r1, r3, r0
	add	r0, r0, r2, asr #2
"))


(define sketch
(send parser ast-from-string "
smmul r0, r0, r1
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
