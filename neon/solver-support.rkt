#lang s-exp rosette

(require "../ast.rkt" "machine.rkt")
(provide encode)

;; type-id

(define (encode code mem-map)

  (define (encode-arg x)
    ;; 32 d, 16 q, 16 r, #constrant, {d0,d1}, {d0-d3}
    (if (vector? x)
        (cons (vector-length x) (vector-map encode-arg x))
        (let ([type (substring x 0 1)])
          (if (member type (list "d" "q" "r"))
              (let ([num (string->number (substring x 1))])
                (cond
                 [(equal? type "d") num]
                 [(equal? type "q") (+ nregs-d num)]
                 [(equal? type "r") num]
                 [else              (raise (format "illegal operand ~a" x))]))
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
    

  (define (sym-type arg)
    (define-symbolic* type number?)
    (assert (and (>= type 0) (< type (vector-length type-id))))
    type)

  (define (first-arg op)
    (define ld-st
      (ormap (lambda (x) (= op (vector-member inst-id x))) '(vld1 vld2 vld1! vld2!)))
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
                (sym-byte x)
                (sym-type x))
        )))

  (traverse code inst? encode-inst))