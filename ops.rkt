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

(define (vector-copy vec [start 0] [end (vector-length vec)])
  (pretty-display `(vector-copy ,start ,end ,(- end start)))
    ;; (let* ([len (- end start)]
    ;;      [new-vec (make-vector len)])
    ;; (pretty-display `(len ,len ,new-vec))
    ;; (for ([i (in-range len)])
    ;;      (pretty-display `(vector-copy ,i))
    ;;      (vector-set! new-vec i (vector-ref vec (+ start i))))
    ;; new-vec)

  (let ([len (- end start)])
    (for ([i (in-range len)])
         (pretty-display `(vector-copy ,i))
         (vector-ref vec (+ start i))))

  (define ret
  (let ([len (- end start)])
    (for/list ([i (in-range len)])
                (vector-ref vec (+ start i)))))
  (pretty-display `(vector-copy-res ,ret))
  (list->vector ret)
  )