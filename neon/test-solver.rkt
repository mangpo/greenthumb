#lang s-exp rosette

(require "../solver.rkt" "../ast.rkt"
         "parser.rkt" "machine.rkt" "solver-support.rkt" "print.rkt")

(define (sym-arg)
  (define-symbolic* arg number?)
  ;(assert (>= arg 0))
  arg)

(define (sym-const)
  (define-symbolic* const number?)
  (assert (and (>= const 1) (<= const 8)))
  const)

(define code
(ast-from-string "
VEXT.16 d5, d3, d4, #1
")) ;; TODO debug


(define sketch
(ast-from-string "
?
"))

(define encoded-code (encode code #f))
(define encoded-sketch (encode sketch #f))
(define encoded-sketch2 
  (vector (inst (vector-member `vld2 inst-id) 
                (vector (cons (sym-arg) (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg)))
                        (sym-arg) (sym-arg) (sym-arg))
                (sym-const) #f)))

;(counterexample encoded-code encoded-sketch (constraint [dreg 5] [rreg] [mem-all]))
(superoptimize encoded-code encoded-sketch (constraint [dreg 5 6] [rreg 2] [mem-all]))