#lang s-exp rosette

(require "vpe/machine.rkt")
(provide (all-defined-out))

(define (finitize num) 
  (match (coerce num number?)
         [(? sym? v) v]
         [v (let* ([mask (arithmetic-shift -1 bit)]
                   [masked (bitwise-and (bitwise-not mask) v)])
              (if (bitwise-bit-set? masked (- bit 1))
                  (bitwise-ior mask masked)  
                  masked))]))