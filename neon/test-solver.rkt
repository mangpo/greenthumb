#lang s-exp rosette

(require "neon-symbolic.rkt" "neon-validator.rkt" "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt" "neon-inst.rkt")

(define (sym-len)
  (define-symbolic* arg number?)
  (assert (and (>= arg 1) (>= arg 4)))
  arg)

(define (sym-arg)
  (define-symbolic* arg number?)
  ;(assert (>= arg 0))
  arg)

(define (sym-const)
  (define-symbolic* const number?)
  (assert (and (>= const -16) (<= const 16)))
  const)

(define (sym-byte)
  (define-symbolic* byte number?)
  (assert (and (>= byte 1) (<= byte 8)))
  byte)

#|(define (sym-type)
  (define-symbolic* type number?)
  (assert (and (>= type 0) (< type (vector-length type-id))))
  type)|#

(define parser (new neon-parser%))
(define machine (new neon-machine%))
(define printer (new neon-printer% [machine machine]))
(define validator (new neon-validator% [machine machine] [printer printer] [simulator simulator-rosette]))
(define symbolic (new neon-symbolic% [machine machine] [printer printer] [parser parser]))

(define code
(send parser ast-from-string "
vorr q3, q0, q0
vld1 {d4,d5}, [r2]
vmov q1, q4
vswp d2, d3
vbsl q3, q1, q2
vst1.32	{d6,d7}, [r2]
")) ;; vmlal.s16 q0, d2, d3[1]


(define sketch
(send parser ast-from-string "
vld1 {d4,d5}, [r2]
vorr d2, d9, d9
vorr d3, d8, d8
vbsl q0, q1, q2
vst1.32	{d0,d1}, [r2]
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send validator encode-sym sketch))
(define encoded-sketch2 
  (vector (neon-inst (send machine get-inst-id `vtrn)
                     (vector (sym-arg) (sym-arg))
                     (sym-byte) #f)))

(send printer print-struct encoded-code)
(send printer print-struct encoded-sketch)
(send printer print-struct encoded-sketch2)

(define t (current-seconds))
;(define x (send validator counterexample encoded-code encoded-sketch 
;                (constraint machine [dreg] [rreg 1 2] [mem-all])))
(send symbolic synthesize-from-sketch encoded-code encoded-sketch 
      (constraint machine [dreg] [rreg] [mem-all]) 9)
(pretty-display `(time ,(- (current-seconds) t)))