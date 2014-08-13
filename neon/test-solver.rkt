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
vld1 {d0}, [r0]
")) ;; TODO debug


(define sketch
(ast-from-string "
?
"))

(define encoded-code (encode code #f))
(define encoded-sketch (encode sketch #f))
(define encoded-sketch2 
  (vector (inst (vector-member `vmlal inst-id) 
                (vector 10 3 2 (sym-arg))
                (sym-byte) (sym-type))))

(define t (current-seconds))
;(counterexample encoded-code encoded-sketch (constraint [dreg 5] [rreg] [mem-all]))
(superoptimize encoded-code encoded-sketch (constraint [dreg 0 1 5] [rreg] [mem-all]))
(pretty-display `(time ,(- (current-seconds) t)))