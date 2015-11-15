#lang racket

(require "../ast.rkt" "../enumerator.rkt")
(require racket/generator)

(provide GA-enumerator%)

(define GA-enumerator%
  (class enumerator%
    (super-new)
    (init-field machine printer)
    (override get-flag generate-inst)
    
    (define inst-id (get-field inst-id machine))
    (define arith-inst
      (map (lambda (x) (vector-member x inst-id)) '(+* 2* 2/ - + and or drop)))
    (define mem-inst
      (map (lambda (x) (vector-member x inst-id)) '(! !b @ @b)))

    (define-syntax-rule (min-list x) (foldl min (car x) (cdr x)))
    (define-syntax-rule (max-list x) (foldl max (car x) (cdr x)))
    
    ;; Since we don't use live-in and live-out to prune the search space here, we just return #f for both of them.
    (define (generate-inst 
             live-in live-out flag-in flag-out
             #:no-args [no-args #f] #:try-cmp [try-cmp #f])
      (define const-range (get-field const-range machine))
      ;; (define inst-choice '(drop @p b! !b))
      ;; (define inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice))
      (define inst-pool (get-field inst-pool machine))
      (when no-args
            (set! inst-pool
                  (filter (lambda (x) (member x arith-inst)) inst-pool)))
      (when (and flag-in flag-out)
            (cond
             [(= (add1 flag-in) (min-list flag-out)) 
	      (set! inst-pool (filter (lambda (x) (member x mem-inst)) inst-pool))]
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
                     (yield (list (inst opcode-id c) #f #f)))]
               [else
                (yield (list (inst opcode-id #f) #f #f))])))
       (yield (list #f #f #f))))

    (define (get-flag state-vec) (length (vector-ref state-vec 9)))
    
    ))
