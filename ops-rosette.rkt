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

(define (smmul x y bit)
  (define byte2 (quotient bit 2))
  (define low-mask (sub1 (arithmetic-shift 1 byte2)))

  (define o1 (bitwise-and x low-mask))
  (define o2 (>> x byte2))
  (define o3 (bitwise-and y low-mask))
  (define o4 (>> y byte2))

  (define o5 (* o1 o3))
  (define o6 (* o2 o3))
  (set! o1 (* o1 o4))
  (set! o2 (* o2 o4))

  (set! o5 (+ o6 (>> o5 byte2)))
  (set! o6 (bitwise-and o5 low-mask))
  (set! o5 (>> o5 byte2))

  (finitize (+ o5 o2 (>> (+ o1 o6) byte2)) bit))

(define (ummul x y bit)
  (define byte2 (quotient bit 2))
  (define low-mask (sub1 (arithmetic-shift 1 byte2)))

  (define x-lo (bitwise-and x low-mask))
  (define x-hi (sym/>>> x byte2))
  (define y-lo (bitwise-and y low-mask))
  (define y-hi (sym/>>> y byte2))
  (define carry 
    (>> (+ (sym/>>> (* x-lo y-lo) byte2)
           (bitwise-and (* x-lo y-hi) low-mask)
           (bitwise-and (* x-hi y-lo) low-mask))
        byte2))
  (define high 
    (+ (* x-hi y-hi) (sym/>>> (* x-lo y-hi) byte2) (sym/>>> (* x-hi y-lo) byte2) carry))
  (finitize high bit))