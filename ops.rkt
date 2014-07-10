#lang s-exp rosette

(provide (all-defined-out))

(define (finitize num) 
  (match (coerce num number?)
         [(? sym? v) v]
         [v (let* ([bitwidth (configured bitwidth)]
                   [mask (arithmetic-shift -1 bitwidth)]
                   [masked (bitwise-and (bitwise-not mask) v)])
              (if (bitwise-bit-set? masked (- bitwidth 1))
                  (bitwise-ior mask masked)  
                  masked))]))