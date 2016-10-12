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
    
    (define/override (filter-with-pruning-info opcode-pool prune-in prune-out
                                        #:try-cmp [try-cmp #f] #:no-args [no-args #f])
      (define-syntax-rule (min-list x) (foldl min (car x) (cdr x)))
      (define-syntax-rule (max-list x) (foldl max (car x) (cdr x)))
      (cond
       [(and prune-in prune-out)
        (cond
         [(= (add1 prune-in) (min-list prune-out)) 
          (filter (lambda (x) (member x mem-inst)) opcode-pool)]
         [(< prune-in (min-list prune-out)) (list)]
         [(> prune-in (max-list prune-out)) (list)]
         [else opcode-pool]
         )
        ]

       [else opcode-pool]))

    (define/override (get-pruning-info state-vec) (get-field index (progstate-comm state-vec)))
    
    ))
