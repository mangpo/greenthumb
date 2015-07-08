#lang racket

(require "../ast.rkt")
(require racket/generator)

(provide GA-enumerative%)

(define GA-enumerative%
  (class object%
    (super-new)
    (init-field machine printer)
    (public get-flag generate-inst)
    
    (define inst-id (get-field inst-id machine))
    (define arith-inst
      (map (lambda (x) (vector-member x inst-id)) '(+* 2* 2/ - + and or drop)))
    (define mem-inst
      (map (lambda (x) (vector-member x inst-id)) '(! !b @ @b)))

    (define-syntax-rule (min-list x) (foldl min (car x) (cdr x)))
    (define-syntax-rule (max-list x) (foldl max (car x) (cdr x)))
    
    ;; Since we don't use live-in to prune the search space here, we just return #f.
    (define (generate-inst 
             live-in live-out flag-in flag-out
             regs type lex #:no-args [no-args #f] #:try-cmp [try-cmp #f])
      (define const-range (get-field const-range machine))
      (define inst-pool (get-field inst-pool machine))
      (when no-args
            (set! inst-pool
                  (filter (lambda (x) (member x arith-inst)) inst-pool)))
      (when (and flag-in flag-out)
            (cond
             [(= (add1 flag-in) (min-list flag-out)) (set! inst-pool mem-inst)]
             [(< flag-in (min-list flag-out)) (set! inst-pool (list))]
             [(> flag-in (max-list flag-out)) (set! inst-pool (list))]
             ))
              
      (generator 
       ()
       (for ([opcode-id inst-pool])
            (let ([opcode-name (vector-ref inst-id opcode-id)])
              (cond
               [(equal? opcode-name `nop) (void)]
               [(equal? opcode-name `@p)
                (for ([c const-range])
                     (yield (list (inst opcode-id c) #f #f 0)))]
               [else
                (yield (list (inst opcode-id #f) #f #f 0))])))
       (yield (list #f #f #f #f))))

    (define (get-flag state-vec) (length (vector-ref state-vec 9)))
    
    ))
