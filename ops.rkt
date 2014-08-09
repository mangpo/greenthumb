#lang s-exp rosette

(require "neon/machine.rkt")
(provide (all-defined-out))

(define (finitize num [bit bit]) 
  (match (coerce num number?)
         [(? sym? v) v]
         [v (let* ([mask (arithmetic-shift -1 bit)]
                   [masked (bitwise-and (bitwise-not mask) v)])
              (if (bitwise-bit-set? masked (- bit 1))
                  (bitwise-ior mask masked)  
                  masked))]))

(define (vector-copy! dest dest-start src 
                      [src-start 0] [src-end (vector-length src)])
  ;(pretty-display `(vector-copy! ,dest-start ,dest))
  (for ([i (in-range (- src-end src-start))])
       (vector-set! dest (+ dest-start i)
                    (vector-ref src (+ src-start i))))
  ;(pretty-display `(res ,dest))
  )