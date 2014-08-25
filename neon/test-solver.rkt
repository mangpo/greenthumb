#lang s-exp rosette

(require "neon-solver.rkt" "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt" "neon-ast.rkt")

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
(define solver (new neon-solver% [machine machine] [printer printer]))

(define code
(send parser ast-from-string "
vtrn.16 d0, d1
")) ;; vmlal.s16 q0, d2, d3[1]


(define sketch
(send parser ast-from-string "
?
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))
(define encoded-sketch2 
  (vector (neon-inst (send machine get-inst-id `vtrn)
                     (vector (sym-arg) (sym-arg))
                     (sym-byte) #f)))

(send printer print-struct encoded-code)
(send printer print-struct encoded-sketch)
(send printer print-struct encoded-sketch2)

(define t (current-seconds))
;(define x (send solver counterexample encoded-code encoded-sketch 
;                (constraint machine [dreg 0 1 3] [rreg 1 2] [mem-all])))
(send solver superoptimize encoded-code encoded-sketch 
      (constraint machine [dreg 0 1] [rreg 0] [mem-all]) #f)
(pretty-display `(time ,(- (current-seconds) t)))