#lang racket

(require "../ast.rkt" "machine.rkt")

(provide print-struct print-syntax
         encode encode-inst
         decode decode-inst
         (all-defined-out))

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
  (inst (vector-member (string->symbol (inst-op x)) inst-id)
        (vector-map encode-arg (inst-args x))
        (and (inst-byte x) 
             (quotient (string->number (inst-byte x)) 8))
        (and (inst-type x) 
             (vector-member 
              (string->symbol (string-downcase (inst-type x))) type-id))))

(define (encode code)
  (for/vector ([i code]) (encode-inst i)))

(define (decode-inst x)
  (define opcode (vector-ref inst-id (inst-op x)))
  (define args (inst-args x))
  (define byte (inst-byte x))
  (define type (inst-type x))
  
  (define-syntax-rule (make-inst type byte x ...)
    (make-inst-main type byte (list x ...)))
  
  (define (make-inst-main type byte fs)
    (define new-args (for/vector ([f fs] [arg args]) (f arg)))
    (inst (symbol->string opcode) new-args byte type))
  
  (define (dreg x) (if (< x nregs-d) (format "d~a" x) (format "q~a" (- x nregs-d))))
  (define (rreg x) (format "r~a" x))
  (define (imm x) (number->string x))
  (define (load-dregs x) (vector-map dreg (vector-take (cdr x) (car x))))
  
  (cond
   [(member opcode '(nop))
    (inst "nop" (vector) #f #f)]
   
   [(member opcode '(vld1 vld2))
    (make-inst #f byte load-dregs rreg)]
   
   [(member opcode '(vld1! vld2!))
    (make-inst #f byte load-dregs rreg)]
   
   [(member opcode '(vmovi vandi))
    (make-inst #f #f dreg imm)] ;; TODO: they do have type & byte
   
   [(member opcode '(vmov))
    (make-inst #f #f dreg dreg)]
   
   [(member opcode '(vmla vmlal vand))
    (make-inst type byte dreg dreg dreg)]
   
   [(member opcode '(vmla# vmlal#))
    (make-inst type byte dreg dreg dreg imm)]
   
   [(member opcode '(vext#))
    (make-inst #f byte dreg dreg dreg imm)]
   
   [else (raise (format "decode-inst: undefined for ~a" opcode))]))

(define (decode code)
  (for/vector ([i code]) (decode-inst i)))

(define (print-struct x [indent ""])
  (define (inc ind) (string-append ind " "))
  (cond

   [(or (list? x) (vector? x))
    (for ([i x]) (print-struct i indent))]

   [(inst? x)
    (define op (inst-op x))
    (pretty-display (format "~a(inst ~a ~a ~a ~a)" indent 
                            ;; (if (number? op)
                            ;;     (vector-ref inst-id (inst-op x))
                            ;;     op)
                            op (inst-args x)
                            (inst-byte x) (inst-type x)))]

   [(block? x)
    (print-struct (block-body x) indent)]

   [(label? x)
    (pretty-display (format "~a(label ~a" indent (label-name x)))
    (print-struct (label-body x) (inc indent))
    (pretty-display (format "~a)" indent))]

   [(program? x)
    (print-struct (program-code x))]
   
   [else
    (pretty-display (format "~a~a" indent x))]))

(define (syntax-opcode opcode)
  (define pos (sub1 (string-length opcode)))
  (define last-alp (substring opcode pos))
  (if (or (equal? last-alp "!") (equal? last-alp "i"))
      (substring opcode 0 pos)
      opcode))

(define (print-syntax x [indent ""])
  (define (opcode-syntax op)
    (define pos (sub1 (string-length op)))
    (if (or (equal? (substring op pos) "!")
            (equal? (substring op pos) "#"))
        (substring op 0 pos)
        op))
    
  (define (inc ind) (string-append ind " "))
  (define (f x indent)
    (cond
     [(inst? x)
      (define opcode (string->symbol (inst-op x)))
      (define args (inst-args x))
      (define byte (inst-byte x))
      (define type (inst-type x))

      (display (format "~a~a" indent (opcode-syntax (inst-op x))))
      (when byte
            (display ".")
            (cond
             [(number? type) (display (vector-ref type-id type))]
             [(string? type) (display type)])
            (cond
             [(number? byte) (display (* 8 byte))]
             [(string? byte) (display byte)]))
      (display " ")

      (cond
       [(member opcode '(vld1 vld2 vld1! vld2!))
        (display (format "{~a} , [~a]"
                         (string-join (vector->list (vector-ref args 0)) ", ")
                         (vector-ref args 1)))
        (when (member opcode '(vld1! vld2!))
              (display "!"))]

       [(member opcode '(vmovi vandi vext#))
        (define last-pos (sub1 (vector-length args)))
        (display 
         (format "~a, #~a"
                 (string-join (take (vector->list args) last-pos) ", ")
                 (vector-ref args last-pos)))]

       [(member opcode '(vmla# vmlal#))
        (define last-pos (sub1 (vector-length args)))
        (display 
         (format "~a[~a]"
                 (string-join (take (vector->list args) last-pos) ", ")
                 (vector-ref args last-pos)))]
       
       [else 
        (display (string-join (vector->list args) ", "))])
      (newline)
      ]
     
     [(or (list? x) (vector? x))
      (for ([i x]) (f i indent))]
     
     [(block? x)
      (when (block-info x)
            (pretty-display (format "~a; ~a" indent (block-info x))))
      (f (block-body x) indent)]
     
     [(label? x)
      (when (label-name x) 
            (pretty-display (format "~a~a" indent (label-name x))))
      (f (label-body x) (inc indent))]
     
     [(program? x)
      (f (program-code x))]
     
     [else
      (pretty-display (format "~a~a" indent x))]))
  (f x indent))
    