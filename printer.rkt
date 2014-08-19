#lang racket

(require "ast.rkt")

(provide printer%)

(define printer%
  (class object%
    (super-new)
    (init-field [report-mutations '#(opcode operand swap inst nop)])
    (public encode decode print-struct print-syntax)
    (abstract print-struct-inst print-syntax-inst
              encode-inst decode-inst)

    (define (encode code)
      (for/vector ([i code]) (encode-inst i)))

    (define (decode code)
      (for/vector ([i code]) (decode-inst i)))

    (define (print-struct x [indent ""])
      (define (inc ind) (string-append ind " "))
      (cond
       [(or (list? x) (vector? x))
        (for ([i x]) (print-struct i indent))]
       
       [(inst? x) (print-struct-inst x indent)]
       
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

    (define (print-syntax x [indent ""])
      (define (inc ind) (string-append ind " "))
      (define (f x indent)
        (cond
         [(inst? x) (print-syntax-inst x indent)]
         
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

    ))
    