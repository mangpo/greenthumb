#lang s-exp rosette

(require (only-in rosette [<< sym/<<] [>>> sym/>>>]))

(provide (all-defined-out))

(define-syntax-rule (assert-return c message val)
  (begin
    (assert c message)
    val))

(define-syntax-rule (<< x y bit) (sym/<< x y))
(define-syntax-rule (>>> x y bit) (sym/>>> x y))

(define (finitize num bit)
  (match (coerce num number?)
         [(? term? v) v]
         [v (let* ([mask (arithmetic-shift -1 bit)]
                   [masked (bitwise-and (bitwise-not mask) v)])
              (if (bitwise-bit-set? masked (- bit 1))
                  (bitwise-ior mask masked)  
                  masked))]))

;; (define (finitize num bit)
;;   (let* ([mask (sym/<< -1 bit)]
;;          [masked (bitwise-and (bitwise-not mask) num)])
;;     (if (= (bitwise-and masked (arithmetic-shift 1 (sub1 bit))) 0)
;;         masked
;;         (bitwise-ior mask masked))))

;; This function is very memory expensive in Rosette
(define (vector-copy! dest dest-start src 
                      [src-start 0] [src-end (vector-length src)])
  ;;(pretty-display `(vector-copy! ,src-start ,src-end ,(- src-end src-start)))
  (for ([i (in-range (- src-end src-start))])
       ;;(pretty-display `(copy-* ,(quotient (current-memory-use) 1000)))
       (vector-set! dest (+ dest-start i)
                    (vector-ref src (+ src-start i))))
  )



;; This function is very memory expensive in Rosette
(define (vector-copy-len! dest dest-start 
                          src src-start len)
  (for ([i (in-range len)])
       ;;(pretty-display `(copy-* ,i))
       (vector-set! dest (+ dest-start i)
                    (vector-ref src (+ src-start i))))
  )

;; TODO: do we need this?
(define (vector-copy-len vec start len)
  ;(pretty-display `(vector-copy ,start ,len))
  (for/vector ([i len]) (vector-ref vec (+ start i))))
  

(define (vector-extract a b shift)
  ;(pretty-display `(vector-extract ,a ,b ,shift))
  (define len (vector-length a))
  (define pos (- len shift))
  (define vec (make-vector len))
  (for ([i (in-range pos)])
       ;(pretty-display `(first ,i ,(+ shift i)))
       (vector-set! vec i (vector-ref a (+ shift i))))
  (for ([i (in-range shift)])
       ;(pretty-display `(second ,(+ pos i) ,i))
       (vector-set! vec (+ pos i) (vector-ref b i)))
  ;(pretty-display `(vector-extract-ret ,vec))
  vec)
       