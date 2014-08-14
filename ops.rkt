#lang s-exp rosette

(require "neon/machine.rkt")
(provide (all-defined-out))

;; (define (finitize num [bit bit]) 
;;   (match (coerce num number?)
;;          [(? sym? v) v]
;;          [v (let* ([mask (arithmetic-shift -1 bit)]
;;                    [masked (bitwise-and (bitwise-not mask) v)])
;;               (if (bitwise-bit-set? masked (- bit 1))
;;                   (bitwise-ior mask masked)  
;;                   masked))]))

(define (finitize num [bit bit]) 
  (let* ([mask (<< -1 bit)]
         [masked (bitwise-and (bitwise-not mask) num)])
    (if (= (bitwise-and masked (arithmetic-shift 1 (sub1 bit))) 0)
        masked
        (bitwise-ior mask masked))))

(define (vector-copy! dest dest-start src 
                      [src-start 0] [src-end (vector-length src)])
  (for ([i (in-range (- src-end src-start))])
       (vector-set! dest (+ dest-start i)
                    (vector-ref src (+ src-start i))))
  )

;; TODO: do we need this?
(define (vector-copy-len vec start len)
  ;(pretty-display `(vector-copy ,start ,len))
  (for/vector ([i len]) (vector-ref vec (+ start i))))
  

(define (vector-extract a b shift)
  (define len (vector-length a))
  (define vec (make-vector len))
  (for ([i (in-range (- len shift))])
       (vector-set! vec i (vector-ref a (+ shift i))))
  (for ([i (in-range shift)])
       (vector-set! vec (+ shift i) (vector-ref b i)))
  vec)
       