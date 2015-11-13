#lang racket

(require "../printer.rkt" 
         "../ast.rkt")

(provide llvm-demo-printer%)

(define llvm-demo-printer%
  (class printer%
    (super-new)
    (inherit-field machine)
    (override encode-inst decode-inst print-syntax-inst) ;; print-struct-inst

    (define (print-syntax-inst x [indent ""])
      (define op (inst-op x))
      (define args (inst-args x))
      (display (format "~a = ~a i32 ~a" 
			      (vector-ref args 0)
			      (if (regexp-match #rx"#" op)
				  (substring op 0 (sub1 (string-length op)))
				  op)
			      (vector-ref args 1)))
      (for ([i (range 2 (vector-length args))])
	   (display (format ", ~a" (vector-ref args i))))
      (newline))
				  
    ;; TODO
    (define (encode-inst x) x)
    (define (decode-inst x) x)

    ))
