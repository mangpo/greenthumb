#lang racket

(require "../ast.rkt")

(provide (all-defined-out))

(define (print-struct x [indent ""])
  (define (inc ind) (string-append ind " "))
  (cond

   [(or (list? x) (vector? x))
    (for ([i x]) (print-struct i indent))]

   [(inst? x)
    (define op (inst-op x))
    (pretty-display (format "~a(inst ~a ~a ~a)" indent 
                            ;; (if (number? op)
                            ;;     (vector-ref inst-id (inst-op x))
                            ;;     op)
                            op
                            (inst-type x) (inst-args x)))]

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