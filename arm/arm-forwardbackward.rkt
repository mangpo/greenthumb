#lang racket

(require "../forwardbackward.rkt")

(provide arm-forwardbackward%)

(define arm-forwardbackward%
  (class forwardbackward%
    (super-new)
    (inherit-field machine)
    (override vector->id)

    (define bit (get-field bit machine))
    (define max-val (arithmetic-shift 1 bit))
    (define mask (sub1 (arithmetic-shift 1 bit)))

    (define (vector->id state)
      ;; (define z (vector-ref state 2))
      (define regs (vector-ref state 0))
      (define id 0)
        
      (for ([r regs]) (set! id (+ (* id max-val) (bitwise-and r mask))))
      
      ;; (+ id (* (vector-member z z-range-db) (power max-val nregs))))
      id)

    ))
