#lang racket

(require "../inst.rkt" "../enumerator.rkt" "GA-machine.rkt")
(require racket/generator)

(provide GA-enumerator%)

(define GA-enumerator%
  (class enumerator%
    (super-new)
    (inherit-field machine)

    (define opcodes (get-field opcodes machine))
    (define mem-inst
      (map (lambda (x) (vector-member x opcodes)) '(! !+ !b @ @+ @b)))
    
    (define/override (filter-with-flags opcode-pool flag-in flag-out #:try-cmp [try-cmp #f])
      (define-syntax-rule (min-list x) (foldl min (car x) (cdr x)))
      (define-syntax-rule (max-list x) (foldl max (car x) (cdr x)))
      (cond
       [(and flag-in flag-out)
        (cond
         [(= (add1 flag-in) (min-list flag-out)) 
          (filter (lambda (x) (member x mem-inst)) opcode-pool)]
         [(< flag-in (min-list flag-out)) (list)]
         [(> flag-in (max-list flag-out)) (list)]
         [else opcode-pool]
         )
        ]

       [else opcode-pool]))

    (define/override (get-flag state-vec) (get-field index (progstate-comm state-vec)))
    
    ))
