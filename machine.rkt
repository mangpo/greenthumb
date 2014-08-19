#lang racket

(provide debug machine%)

(define debug #f)

(define machine%
  (class object%
    (super-new)
    (init-field [bit #f] [inst-id #f] [classes #f] [classes-len #f] [perline 8])
    (abstract set-config set-machine-config-string
              get-state 
              adjust-config config-exceed-limit?
              output-constraint-string)
    (public get-class-id print-line no-assumption)

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

    ))