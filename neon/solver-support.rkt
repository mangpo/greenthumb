#lang s-exp rosette

(require "../ast.rkt" "machine.rkt")
(provide encode evaluate-state
         assume assert-output)

(define (evaluate-state state sol)
  (define dregs (vector-copy (progstate-dregs state)))
  (define rregs (vector-copy (progstate-rregs state)))
  (define memory (vector-copy (progstate-memory state)))
  (define cost (progstate-cost state))

  (define-syntax-rule (eval x model)
    (let ([ans (evaluate x model)])
      (if (sym? ans) 0 ans)))
  
  (for ([i (vector-length dregs)]
        [dreg dregs])
       (vector-set! dregs i (eval dreg sol)))
  
  (for ([i (vector-length rregs)]
        [rreg rregs])
       (vector-set! rregs i (eval rreg sol)))
  
  (for ([i (vector-length memory)]
        [mem memory])
       (vector-set! memory i (eval mem sol)))

  (progstate dregs rregs memory cost))

(define (encode code mem-map)

  (define (encode-arg x)
    ;; 32 d, 16 q, 16 r, #constrant, {d0,d1}, {d0-d3}
    (if (vector? x)
        (cons (vector-length x) (vector-map encode-arg x))
        (let ([type (substring x 0 1)])
          (if (member type (list "d" "q" "r" "#"))
              (let ([num (string->number (substring x 1))])
                (cond
                 [(equal? type "q") (+ nregs-d num)]
                 [else num]))
              (string->number x)))))

  (define (sym-op)
    (define-symbolic* op number?)
    (assert (and (>= op 0) (< op (vector-length inst-id))))
    op)

  (define (sym-arg)
    (define-symbolic* arg number?)
    arg)

  (define (sym-byte)
    (define-symbolic* byte number?)
    (assert (and (>= byte 1) (<= byte 8)))
    byte)
    

  (define (sym-type)
    (define-symbolic* type number?)
    (assert (and (>= type 0) (< type (vector-length type-id))))
    type)

  (define (first-arg op)
    (define ld-st
      (ormap (lambda (x) (= op (vector-member x inst-id))) '(vld1 vld2 vld1! vld2!)))
    (if ld-st
        (cons (sym-arg) (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg)))
        (sym-arg)))

  (define (encode-inst x)
    (if (inst-op x)
        (inst (vector-member (string->symbol (string-downcase (inst-op x))) inst-id)
              (vector-map encode-arg (inst-args x))
              (and (inst-byte x) (quotient (string->number (inst-byte x)) 8))
              (and (inst-type x)
                   (vector-member (string->symbol (string-downcase (inst-type x))) type-id)))
        (let ([op (sym-op)])
          (inst op
                (vector (first-arg op) (sym-arg) (sym-arg) (sym-arg))
                (sym-byte)
                (sym-type))
        )))

  (traverse code inst? encode-inst))


(define (assert-output state1 state2 constraint cost)
  (when debug (pretty-display "start assert-output"))
  (define dregs (progstate-dregs constraint))
  (define rregs (progstate-rregs constraint))
  (define memory (progstate-memory constraint))

  (define dregs1 (progstate-dregs state1))
  (define rregs1 (progstate-rregs state1))
  (define memory1 (progstate-memory state1))
  (define cost1 (progstate-cost state1))

  (define dregs2 (progstate-dregs state2))
  (define rregs2 (progstate-rregs state2))
  (define memory2 (progstate-memory state2))
  (define cost2 (progstate-cost state2))
  
  (for ([d dregs]
	[d1 dregs1]
	[d2 dregs2])
       (when d (assert (equal? d1 d2))))
  
  (for ([r rregs]
	[r1 rregs1]
	[r2 rregs2])
       (when r (assert (equal? r1 r2))))

  (for ([m memory]
	[m1 memory1]
	[m2 memory2])
       (when m (assert (equal? m1 m2))))

  (when cost (assert (< cost2 cost 1)))
  (when debug (pretty-display "end assert-output"))
  )

;; Default: no assumption
(define (assume state assumption)
  (when assumption
	(raise "No support for assumption")))