#lang racket

(provide bit nregs-d nregs-r
         inst-id type-id
         (all-defined-out))

(struct progstate (dregs rregs memory cost))

(define debug #f)
(define bit 32)
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
                        vand vandi))

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
                (make-vec (* 8 nmems) init)
                0))

    ((default-state init [dreg (a b) ...] [rreg (c d) ...] [mem (e f) ...])
     (let* ([state (default-state init)]
            [dregs (progstate-dregs state)]
            [rregs (progstate-rregs state)]
            [memory (progstate-memory state)])
       (vector-set! dregs a b)
       ...
       (vector-set! rregs c d)
       ...
       (vector-set! memory e f)
       ...
       state))))

;; Macros to create output state constraint
(define-syntax constraint
  (syntax-rules (all none dreg rreg mem mem-all)
    ((constraint all) (default-state #t))

    ((constraint none) (default-state #f))

    ((constraint [dreg d ...] [rreg r ...] [mem-all])
     (let* ([state (progstate (make-vec (* 8 nregs-d) #f) 
                              (make-vec nregs-r #f) 
                              (make-vec (* 8 nmems) #t)
                              0)]
            [dregs (progstate-dregs state)]
            [rregs (progstate-rregs state)])
       (for ([i 8]) (vector-set! dregs (+ (* 8 d) i) #t))
       ...
       (vector-set! rregs r #t)
       ...
       state))
    ))

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

;; Macros to create input state assumption
(define-syntax-rule (no-assumption)
  #f)