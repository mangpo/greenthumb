#lang racket

(require "../printer.rkt" 
         "../ast.rkt" "neon-ast.rkt"
         "neon-machine.rkt")
(provide neon-printer%)

(define neon-printer%
  (class printer%
    (super-new)
    (inherit-field machine report-mutations)
    (override print-struct-inst print-syntax-inst
              encode-inst decode-inst)
    (set! report-mutations (vector-append report-mutations '#(byte type)))
    (define nregs-d (get-field nregs-d machine))

    (define (print-struct-inst x [indent ""])
      (define op (inst-op x))
      (pretty-display (format "~a(inst ~a ~a ~a ~a)" indent 
                              op (inst-args x)
                              (inst-byte x) (inst-type x))))
    
    (define (opcode-syntax op)
      (define pos (sub1 (string-length op)))
      (if (or (equal? (substring op pos) "!")
              (equal? (substring op pos) "#")
              (equal? (substring op pos) "@")
              )
          (substring op 0 pos)
          op))
    
    (define (print-syntax-inst x [indent ""])
      (define opcode (string->symbol (inst-op x)))
      (define args (inst-args x))
      (define byte (inst-byte x))
      (define type (inst-type x))

      (display (format "~a~a" indent (opcode-syntax (inst-op x))))
      (when byte
        (display ".")
        (when type (display type))
        (when byte (display byte)))
      (display " ")

      (cond
       [(member opcode '(vld1 vld2 vld1! vld2!))
        (display (format "{~a} , [~a]"
                         (string-join (vector->list (vector-ref args 0)) ", ")
                         (vector-ref args 1)))
        (when (member opcode '(vld1! vld2!))
              (display "!"))]

       [(member opcode '(vmov# vand# vext#))
        (define last-pos (sub1 (vector-length args)))
        (display 
         (format "~a, #~a"
                 (string-join (take (vector->list args) last-pos) ", ")
                 (vector-ref args last-pos)))]

       [(member opcode '(vmla@ vmlal@))
        (define last-pos (sub1 (vector-length args)))
        (display 
         (format "~a[~a]"
                 (string-join (take (vector->list args) last-pos) ", ")
                 (vector-ref args last-pos)))]
       
       [else 
        (display (string-join (vector->list args) ", "))])
      (newline))


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

    (define (encode-inst x)
      (neon-inst 
       (send machine get-inst-id (string->symbol (inst-op x)))
       (vector-map encode-arg (inst-args x))
       (and (inst-byte x) 
            (quotient (string->number (inst-byte x)) 8))
       (and (inst-type x) 
            (send machine get-type-id 
                  (string->symbol (string-downcase (inst-type x)))))))
    
    (define (decode-inst x)
      (define opcode (send machine get-inst-name (inst-op x)))
      ;;(pretty-display `(decode-inst ,opcode))
      (define args (inst-args x))
      (define byte (inst-byte x))
      (define type (inst-type x))
      
      (define-syntax-rule (make-inst type byte x ...)
        (make-inst-main type byte (list x ...)))
      
      (define (make-inst-main type byte fs)
        (define new-args (for/vector ([f fs] [arg args]) (f arg)))
        (neon-inst (symbol->string opcode) 
                   new-args 
                   (and byte (* byte 8))
                   (and type (send machine get-type-name type))))
      
      (define (dreg x) (if (< x nregs-d) (format "d~a" x) (format "q~a" (- x nregs-d))))
      (define (rreg x) (format "r~a" x))
      (define (imm x) (number->string x))
      (define (load-dregs x) (vector-map dreg (vector-take (cdr x) (car x))))
      
      (cond
       [(member opcode '(nop))
        (neon-inst "nop" (vector) #f #f)]
       
       [(member opcode '(vld1 vld2))
        (make-inst #f byte load-dregs rreg)]
       
       [(member opcode '(vld1! vld2!))
        (make-inst #f byte load-dregs rreg)]
       
       [(member opcode '(vmovi vandi))
        (make-inst #f #f dreg imm)] ;; TODO: they do have type & byte
       
       [(member opcode '(vmov))
        (make-inst #f #f dreg dreg)]
       
       [(member opcode '(vmov#))
        (make-inst #f #f dreg imm)]
       
       [(member opcode '(vmla vmlal vand))
        (make-inst type byte dreg dreg dreg)]
       
       [(member opcode '(vmla@ vmlal@))
        (make-inst type byte dreg dreg dreg imm)]
       
       [(member opcode '(vext#))
        (make-inst #f byte dreg dreg dreg imm)]
       
       [else (raise (format "decode-inst: undefined for ~a" opcode))]))

    ))