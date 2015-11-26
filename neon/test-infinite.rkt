#lang s-exp rosette

(require "neon-simulator-rosette.rkt" "neon-machine.rkt" "neon-printer.rkt"
         "neon-parser.rkt" "neon-inst.rkt")

(define (sym-arg)
  (define-symbolic* arg number?)
  ;(assert (>= arg 0))
  arg)

(define (sym-op)
  (define-symbolic* op number?)
  op)

(define (sym-byte)
  (define-symbolic* byte number?)
  (assert (and (>= byte 1) (<= byte 8)))
  byte)

(define parser (new neon-parser%))
(define machine (new neon-machine%))
(define printer (new neon-printer% [machine machine]))
(define simulator (new neon-simulator-rosette% [machine machine]))

(define x (neon-inst (sym-op)
                     (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg))
                     (sym-byte) #f))

(define x-concrete (neon-inst 13
                     (vector 0 1 2)
                     2 2))

;; Non-terminating
(send simulator get-schedule-info x)
;; Terminating of x is concrete
;; (send simulator get-schedule-info x-concrete)