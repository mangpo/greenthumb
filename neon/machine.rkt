#lang racket

(provide bit nregs-d nregs-r
         inst-id type-id
         (all-defined-out))

(struct progstate (dregs rregs memory))

(define bit 64)
(define nregs-d 10)
(define nregs-r 4)
(define nmems 4)

;; symbol
(define inst-id '#(vld1 vld2 ;vld3 vld4
                        vld1! vld2! ;vld3! vld4!
                        vexti 
                        vmla vmlal
                        vmov vmovi vmovl vmovn vqmovn
                        vmvn vmvni
                        vand))

;; string
(define type-id '#(i s u))

(define-syntax-rule (make-vec n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i init))
    vec))

(define-syntax default-state
  (syntax-rules (rreg dreg mem)
    ((default-state init)
     (progstate (make-vec (* 8 nregs-d) init) 
                (make-vec nregs-r init) 
                (make-vec (* 8 nmems) init)))))


(define (display-state s)
  (pretty-display "DREGS:")
  (print-line (progstate-dregs s))
  (pretty-display "RREGS:")
  (print-line (progstate-rregs s))
  (pretty-display "MEMORY:")
  (print-line (progstate-memory s)))

(define (print-line v)
  (define count 0)
  (for ([i v])
       (when (= count 8)
	     (newline)
	     (set! count 0))
       (display i)
       (display " ")
       (set! count (add1 count))
       )
  (newline)
  )