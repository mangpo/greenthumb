#lang racket

(provide (all-defined-out))


(define-syntax-rule (<< x y bit)
  (if (and (>= y 0) (< y bit))
      (let ([mask (sub1 (arithmetic-shift 1 (- bit y)))])
        (finitize (arithmetic-shift (bitwise-and x mask) y) bit))
      0))

(define-syntax-rule (>> x y)
  (if (>= y 0)
      (arithmetic-shift x (- y))
      (if (>= x 0) 0 -1)))

(define-syntax-rule (>>> x y bit)
  (if (= y 0)
      x
      (let ([unsigned-x (bitwise-and x (sub1 (arithmetic-shift 1 bit)))])
        (>> unsigned-x y))))

(define (finitize num bit)
  (let* ([mask (arithmetic-shift -1 bit)]
         [masked (bitwise-and (bitwise-not mask) num)])
    (if (= (bitwise-and masked (arithmetic-shift 1 (sub1 bit))) 0)
        masked
        (bitwise-ior mask masked))))

(define-syntax-rule (get-field* f o) (get-field f o))
(define-syntax-rule (is-a?* o type) (is-a? o type))
(define-syntax-rule (send* o f ...) (send o f ...))

;;;;;;;;;;;;;;;;;;;;; assert ;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (assert-return c message val) val)
(define-syntax assert
  (syntax-rules ()
    ((assert x) 
     (unless x 
       (raise (exn "racket: assert fail" (current-continuation-marks)))))
    ((assert x y) 
     (unless x 
       (raise (exn (format "racket: assert fail: ~a" y) (current-continuation-marks)))))))

(define-syntax-rule (for/all ([a b] ...) expr)
  (let ([a b] ...) expr))
     
(define-syntax-rule (for*/all ([a b] ...) expr)
  (let* ([a b] ...) expr))

;;;;;;;;;;;;;;;;;;;;; vector ;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: do we need this?
(define (vector-copy-len vec start len)
  ;(pretty-display `(vector-copy ,start ,len))
  (for/vector ([i len]) (vector-ref vec (+ start i))))

(define (vector-copy-len! dest dest-start src 
                          src-start len)
  (vector-copy! dest dest-start src src-start (+ src-start len))) 
  

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

;;;;;;;;;;;;;;;;;;;;; lookup table ;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-rule (lookup-table assoc)
  (make-hash assoc))

(define-syntax lt-ref
  (syntax-rules ()
    [(lt-ref table key) (hash-ref table key)]
    [(lt-ref table key default) (hash-ref table key default)]))
       
;;;;;;;;;;;;;;;;;;;;; multiplication ;;;;;;;;;;;;;;;;;;;;;;;;
(define (smmul x y bit) 
  (finitize (arithmetic-shift (* x y) (- bit)) bit))

(define (ummul x y bit) 
  (let ([mask (sub1 (arithmetic-shift 1 bit))])
    (finitize 
     (arithmetic-shift (* (bitwise-and x mask) (bitwise-and y mask)) (- bit)) 
     bit)))
