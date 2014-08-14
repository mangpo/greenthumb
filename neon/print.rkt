#lang racket

(require "../ast.rkt" "machine.rkt")

(provide (all-defined-out))

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

(define (print-syntax x [indent ""] #:print-reg [print-reg #t])
  (define dreg
    (if print-reg
        (lambda (x) (if (< x nregs-d)
                        (format "d~a" x)
                        (format "q~a" (quotient x 2))))
        identity))
  (define load-dregs
    (lambda (x)
      (let ([regs (if (pair? x) 
                      (take (vector->list (cdr x)) (car x))
                      (vector->list x))])
        (format "{~a}" (string-join (map dreg regs) ", ")))))
  (define rreg
    (if print-reg
        (lambda (x) (format "r~a" x))
        identity))
  (define load-rreg (lambda (x) (format "[~a]" (rreg x))))
  (define imm (lambda (x) (format "#~a" x)))

  (define (inc ind) (string-append ind " "))
  (define (f x indent)
    (cond
     [(inst? x)
      (define op (inst-op x))
      (define args (inst-args x))
      (define byte (inst-byte x))
      (define type (inst-type x))
      (define opcode (if (number? op) 
                         (vector-ref inst-id op)
                         (string->symbol op)))
      
      (display (format "~a~a " indent (syntax-opcode (symbol->string opcode))))

      (define (print-inst-main formats [with ""])
        ;; (if byte
        ;;     (begin
        ;;       (display ".")
        ;;       (cond
        ;;        [(number? type) (display (vector-ref type-id type))]
        ;;        [(string? type) (display type)])
        ;;       (cond
        ;;        [(number? byte) (display (* 8 byte))]
        ;;        [(string? byte) (display byte)])
        ;;       )
        ;;     (display " "))
        (define lst
          (for/list ([format formats]
                     [arg args])
                    (format arg)))
        (display (string-join lst ", "))
        (display with)
        (newline))

      (define-syntax print-args
        (syntax-rules (with)
          ((print-args x ... [with a])
           (print-inst-main (list x ...) a))
          ((print-args x ...) (print-inst-main (list x ...)))))

      (cond
       [(member opcode '(nop)) (newline)]

       [(member opcode '(vld1 vld2))
        (print-args load-dregs load-rreg)]

       [(member opcode '(vld1! vld2!))
        (print-args load-dregs load-rreg [with "!"])]

       [(member opcode '(vmovi vandi))
        (print-args dreg imm)]

       [(member opcode '(vmov))
        (print-args dreg dreg)]

       [(member opcode '(vmla vmlal vand))
        (print-args dreg dreg dreg)]

       [(member opcode '(vmlai vmlali))
        (print-args dreg dreg dreg 
                    [with (format "[~a]" (vector-ref args 3))])]

       [(member opcode '(vexti))
        (print-args dreg dreg dreg imm)]
       
       [else (raise (format "print-syntax: undefined fro ~a" opcode))])
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
    