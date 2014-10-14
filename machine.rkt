#lang racket

(require "ast.rkt")
(provide debug machine%)

(define debug #f)

(define machine%
  (class object%
    (super-new)
    (init-field [bit #f] [random-input-bit #f] [inst-id #f] [classes #f] [classes-len #f] [perline 8])
    (abstract set-config get-config set-config-string
              get-state display-state
              adjust-config config-exceed-limit?
              output-constraint-string
              progstate->vector vector->progstate)
    (public get-class-id print-line no-assumption
            get-inst-id get-inst-name
            output-assume-string
            display-state-text parse-state-text get-states-from-file syntax-equal?)

    (define (get-inst-id opcode)
      (vector-member opcode inst-id))

    (define (get-inst-name id)
      (vector-ref inst-id id))

    (define (no-assumption) #f)

    ;; x: name in form of symbol
    (define (get-class-id x)
      (define id #f)
      (for ([i (in-range classes-len)])
           (when (member x (vector-ref classes i))
                 (set! id i)))
      id)

    (define (print-line v)
      (define count 0)
      (for ([i v])
           (when (= count perline)
	     (newline)
	     (set! count 0))
           (display i)
           (display " ")
           (set! count (add1 count))
           )
      (newline)
      )

    (define (output-assume-string machine-var x)
      x)

    (define (display-state-text x)
      (raise "display-state-text: unimplemented"))

    (define (parse-state-text str)
      (raise "parse-state-text: unimplemented"))

    (define (get-states-from-file file)
      (define port (open-input-file file))
      (define (parse)
        (define line (read-line port))
        (if (equal? line eof)
            (list)
            (cons (parse-state-text line)
                  (parse))))
      (define ret (parse))
      (close-input-port port)
      ret)
    
    (define (syntax-equal? code1 code2)
      (for/and ([x1 code1]
                [x2 code2])
               (and (equal? (inst-op x1) (inst-op x2))
                    (equal? (inst-args x1) (inst-args x2)))))

    ))
