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

;; (define (smmul x y bit)
;;   (define byte2 (quotient bit 2))
;;   (define low-mask (sub1 (arithmetic-shift 1 byte2)))

;;   (define o1 (bitwise-and x low-mask))
;;   (define o2 (>> x byte2))
;;   (define o3 (bitwise-and y low-mask))
;;   (define o4 (>> y byte2))

;;   (define o5 (* o1 o3))
;;   (define o6 (* o2 o3))
;;   (set! o1 (* o1 o4))
;;   (set! o2 (* o2 o4))

;;   (set! o5 (+ o6 (>> o5 byte2)))
;;   (set! o6 (bitwise-and o5 low-mask))
;;   (set! o5 (>> o5 byte2))

;;   (finitize (+ o5 o2 (>> (+ o1 o6) byte2)) bit))

;; (define (ummul u v bit)
;;   (define byte2 (quotient bit 2))
;;   (define low-mask (sub1 (arithmetic-shift 1 byte2)))

;;   (define u0 (bitwise-and u low-mask))
;;   (define u1 (bitwise-and (>> u byte2) low-mask))
;;   (define v0 (bitwise-and v low-mask))
;;   (define v1 (bitwise-and (>> v byte2) low-mask))

;;   (finitize
;;    (+ (* u1 v1) 
;;       (sym/>>> (* u1 v0) byte2) 
;;       (sym/>>> (* u0 v1) byte2) 
;;       (sym/>>> (+ (bitwise-and (* u1 v0) low-mask)
;;                   (bitwise-and (* u0 v1) low-mask)
;;                   (sym/>>> (* u0 v0) byte2))
;;                byte2))
;;    bit))

(define (smmul x y bit) 
  (define p (*h x y))
  (define t1 (bitwise-and (>> x (sub1 bit)) y))
  (define t2 (bitwise-and (>> y (sub1 bit)) x))
  (finitize (- p t1 t2) bit))

(define (ummul x y bit) 
  (*h x y))