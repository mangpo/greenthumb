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
      (unless (equal? op "nop")
              (define args (inst-args x))
              (display (format "~a = ~a i32 ~a" 
                               (vector-ref args 0)
                               op
                               ;; (if (regexp-match #rx"#" op)
                               ;;     (substring op 0 (sub1 (string-length op)))
                               ;;     op)
                               (vector-ref args 1)))
              (for ([i (range 2 (vector-length args))])
                   (display (format ", ~a" (vector-ref args i))))
              (newline)))

    (define name2num (make-hash))
    (define num2name (make-vector 100))
    (define n 0)
				  
    ;; Convert from string to number representation.
    (define (encode-inst x)
      (define args (inst-args x))
      (define last-arg (vector-ref args (sub1 (vector-length args))))
      (define new-args
        (for/vector ([arg args])
                    (if (equal? (substring arg 0 1) "%")
                        (if (hash-has-key? name2num arg)
                            (hash-ref name2num arg)
                            (let ([id n])
                              (set! n (add1 n))
                              (hash-set! name2num arg id)
                              (vector-set! num2name id arg)
                              id))
                        (string->number arg))))
      (define op
        (string->symbol
         (if (equal? (substring last-arg 0 1) "%")
             (inst-op x)
             (string-append (inst-op x) "#"))))
      (inst (send machine get-inst-id op) new-args))
    
    
    (define (decode-inst x)
      (define op (symbol->string (send machine get-inst-name (inst-op x))))
      (define args (inst-args x))
      (define last-arg (vector-ref args (sub1 (vector-length args))))
      (if (regexp-match #rx"#" op)
          (set! op (substring op 0 (sub1 (string-length op))))
          (set! last-arg (vector-ref num2name last-arg)))

      (define new-args
        (for/vector ([i (sub1 (vector-length args))])
                    (vector-ref num2name (vector-ref args i))))

      (inst op (vector-append new-args (vector last-arg))))
    

    ))
