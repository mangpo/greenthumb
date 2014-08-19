#lang s-exp rosette

(require "neon-solver.rkt" "neon-machine.rkt" "neon-printer.rkt" "neon-simulator-rosette.rkt"
         "../ast.rkt" "neon-ast.rkt"
         "parser.rkt" )

#|
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

(define (sym-type)
  (define-symbolic* type number?)
  (assert (and (>= type 0) (< type (vector-length type-id))))
  type)|#

(define machine (new neon-machine%))
(define printer (new neon-printer% [machine machine]))
(define simulator (new neon-simulator% [machine machine]))
(define solver (new neon-solver% [machine machine] [printer printer] [simulator simulator]))

(define code
(ast-from-string "
 VLD1.16 {d4}, [r2]! ; 2 cycles (LSBP)
 VLD1.16 {d2}, [r1]! ; 2 cycles (LSBP)
 VEXT.16 d5, d3, d4, #1 ; 1 cycle (LSBP)
 VEXT.16 d6, d3, d4, #2 ; 1 cycle (LSBP)
 VEXT.16 d7, d3, d4, #3 ; 1 cycle (LSBP)
 VMLAL.S16 q0, d3, d2[0] ; 1 cycle (DP)
 VMLAL.S16 q0, d5, d2[1] ; 1 cycle (DP)
 VMLAL.S16 q0, d6, d2[2] ; 1 cycle (DP)
 VMLAL.S16 q0, d7, d2[3] ; 1 cycle (DP)
 VMOV d3, d4 ; 1 cycle (LSBP)
")) ;; TODO debug


(define sketch
(ast-from-string "
 VLD1.16 {d4}, [r2]! ; 2 cycles (LSBP)
 VLD1.16 {d2}, [r1]! ; 2 cycles (LSBP)
 VEXT.16 d5, d3, d4, #1 ; 1 cycle (LSBP)
 VEXT.16 d6, d3, d4, #2 ; 1 cycle (LSBP)
 VEXT.16 d7, d3, d4, #3 ; 1 cycle (LSBP)
 VMLAL.S16 q0, d3, d2[0] ; 1 cycle (DP)
 VMLAL.S16 q0, d5, d2[1] ; 1 cycle (DP)
 VMLAL.S16 q0, d6, d2[2] ; 1 cycle (DP)
 VMLAL.S16 q0, d7, d2[3] ; 1 cycle (DP)
 ?
"))

(define encoded-code (send printer encode code))
(define encoded-sketch (send solver encode-sym sketch))
#|(define encoded-sketch2 
  (vector (inst (vector-member `vmlal inst-id) 
                (vector 10 3 2 (sym-arg))
                (sym-byte) (sym-type))))|#

(send printer print-struct encoded-code)
(send printer print-struct encoded-sketch)

(define t (current-seconds))
;(define x (send solver counterexample encoded-code encoded-sketch 
;                (constraint machine [dreg 0 1 3] [rreg 1 2] [mem-all])))
(send solver superoptimize encoded-code encoded-sketch 
      (constraint machine [dreg 0 1 3] [rreg] [mem-all]))
(pretty-display `(time ,(- (current-seconds) t)))