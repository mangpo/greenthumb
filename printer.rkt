#lang racket

(require "inst.rkt")

(provide printer%)

(define printer%
  (class object%
    (super-new)
    (init-field machine [report-mutations '#(opcode operand swap inst nop)])
    (public encode decode 
            print-struct print-struct-inst
            print-syntax print-syntax-inst
            compress-state-space decompress-state-space
            config-from-string-ir set-config-string
            output-constraint-string output-assume-string
            print-encode-info)
    ;; Required methods to be intialized when extended. See arm/arm-printer.rkt.
    (abstract encode-inst decode-inst)

    (define (encode code)
      (define ret (vector-map (lambda (x) (encode-inst x)) code))
      (print-encode-info)
      ret)

    (define (decode code)
      (vector-map (lambda (x) (decode-inst x)) code))

    (define (print-encode-info) (void))

    (define (print-struct-inst x [indent ""])
      (pretty-display (format "~a(inst ~a ~a)" 
                              indent (inst-op x) (inst-args x))))

    (define (print-syntax-inst x [indent ""])
      (pretty-display 
       (format "~a~a ~a" 
               indent (inst-op x)
               (string-join (vector->list (inst-args x)) ", "))))
      

    (define (print-struct x [indent ""])
      (define (inc ind) (string-append ind " "))
      (cond
       [(or (list? x) (vector? x))
        (for ([i x]) (print-struct i indent))]
       
       [(inst? x) (print-struct-inst x indent)]
       
       [else
        (pretty-display (format "~a~a" indent x))]))

    (define (print-syntax x [indent ""])
      (define (inc ind) (string-append ind " "))
      (define (f x indent)
        (cond
         [(inst? x) (print-syntax-inst x indent)]
         
         [(or (list? x) (vector? x))
          (for ([i x]) (f i indent))]
         
         [else
          (pretty-display (format "~a~a" indent x))]))
      (f x indent))

    ;; Default: no compression
    (define (compress-state-space program live-out)
      (values program
              live-out
              #f
              (config-from-string-ir program)))

    (define (decompress-state-space program reg-map) program)
    (define (config-from-string-ir program)
      (raise "config-from-string-ir: unimplemented. Need to extend this function."))
    (define (output-constraint-string live-out)
      (raise "config-from-string-ir: unimplemented. Need to extend this function."))
    (define (output-assume-string x) x)
    
    (define (set-config-string x)
      (cond
       [(boolean? x) (format "~a" x)]
       [(number? x) (number->string x)]
       [(or (list? x) (vector? x))
        (string-join
         (append (list "(list")
                 (for/list ([i x]) (set-config-string i))
                 (list ")")))]
       [(pair? x)
        (format "(cons ~a ~a)"
                (set-config-string (car x))
                (set-config-string (cdr x)))]
       [else
        (raise (format "printer:set-config-string: unimplemented for ~a" x))]))
    ))
    
