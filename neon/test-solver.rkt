#lang s-exp rosette

(require "../solver.rkt" "../ast.rkt"
         "parser.rkt" "machine.rkt" "solver-support.rkt" "print.rkt")

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
    type)

(define code
(ast-from-string "
vmlal.s16 q0, d3, d2[0]
")) ;; TODO debug


(define sketch
(ast-from-string "
vmlal.u16 q0, d3, d2[0]
"))

(define encoded-code (encode code))
(define encoded-sketch (encode sketch))
(define encoded-sketch2 
  (vector (inst (vector-member `vmlal inst-id) 
                (vector 10 3 2 (sym-arg))
                (sym-byte) (sym-type))))

(print-struct encoded-code)
(print-struct encoded-sketch)

(define t (current-seconds))
(counterexample encoded-code encoded-sketch (constraint [dreg 0 1] [rreg] [mem-all]))
;(superoptimize encoded-code encoded-sketch (constraint [dreg 0 1 5] [rreg] [mem-all]))
(pretty-display `(time ,(- (current-seconds) t)))