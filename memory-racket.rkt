#lang racket

(require "ops-racket.rkt")
(provide memory-racket%)

(define memory-racket%
  (class* object% (equal<%>)
    (super-new)
    (init-field [init (make-hash)]
                [update (make-hash)]
                ;; If this memory object is for interpreting specification program,
                ;; don't initialize ref.
                ;; Otherwise, initailize ref with memory object output from specification program.
                [ref #f])
    (public print load store clone update-equal? correctness-cost
            ;; for backward interpret
            clone-all del lookup-update
            get-update-addr-val get-update-addr-with-val get-addr-with-val
            ;; internal use only
            lookup-init
            )

    (define (print)
      (pretty-display (format "init: ~a" init))
      (pretty-display (format "update: ~a" update)))

    (define/public (equal-to? other recur)
      (equal? update (get-field update other)))

    (define/public (equal-hash-code-of hash-code)
      (hash-code update))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code update))
      
    ;; Clone a new memory object with the same init.
    ;; Use this method to clone new memory for every program interpretation.
    (define (clone [ref #f])
      (new memory-racket% [ref ref] [init init]))
      
    ;; Clone a new memory object with the same update.
    ;; Use this method to clone new memory for every inverse program interpretation.
    (define (clone-all [ref #f])
      (new memory-racket% [ref ref] [init init]
           [update (make-hash (hash->list update))]))

    (define (update-equal? other) (equal? update (get-field update other)))

    (define (correctness-cost other diff-cost bit)
      (define cost 0)
      (for ([pair (hash->list update)])
           (let* ([addr (car pair)]
                  [val (cdr pair)]
                  [other-val (or (send other lookup-update addr)
                                 (send other lookup-init addr))])
             (set! cost (+ cost
                           (if other-val
                               (diff-cost val other-val)
                               bit)))))
      cost)

    ;;;;;;;;;;;;;;;;;;;; get addr & val ;;;;;;;;;;;;;;;;;;;;;
    (define (get-update-addr-val)
      (hash->list update))
    
    (define (get-update-addr-with-val val)
      (map car (filter (lambda (x) (= (cdr x) val)) (hash->list update))))

    (define (get-addr-with-val val)
      (append
       (map car (filter (lambda (x) (= (cdr x) val)) (hash->list update)))
       (map car (filter (lambda (x)
                          (and (= (cdr x) val)
                               (not (hash-has-key? update (car x)))))
                        (hash->list init)))))
    

    ;;;;;;;;;;;;;;;;;;;; lookup & update ;;;;;;;;;;;;;;;;;;;;
    (define (lookup storage addr)
      (and (hash-has-key? storage addr)
           (hash-ref storage addr)))
    
    (define (lookup-init addr) (lookup init addr))
    (define (lookup-update addr) (lookup update addr))

    (define (modify storage addr val)
      (hash-set! storage addr val))

    ;;;;;;;;;;;;;;;;;;;; del ;;;;;;;;;;;;;;;;;;;;
    (define (del addr)
      (hash-remove! update addr))
      
    ;;;;;;;;;;;;;;;;;;;; load ;;;;;;;;;;;;;;;;;;;;
    
    (define (load-spec addr)
      ;;(pretty-display `(load-spec ,init ,(lookup init addr)))
      (or (lookup update addr)
          (lookup init addr)
          (assert #f "load illegal address (spec)")))

    (define (load-cand addr ref-mem)
      (or (lookup update addr)
          (send ref-mem lookup-init addr)
          (assert #f "load illegal address (candidate)")))

    (define (load addr)
      (if ref
          (load-cand addr ref)
          (load-spec addr)))
    
    ;;;;;;;;;;;;;;;;;;;; store ;;;;;;;;;;;;;;;;;;;;
    
    (define (store-spec addr val)
      (modify update addr val))

    (define (store-cand addr val mem-ref)
      ;; legal to update if that address is used for spec.
      (cond
        [(send mem-ref lookup-update addr)
         (store-spec addr val)]
        [else (assert #f "store illegal address")]))
      
    
    (define (store addr val)
      (if ref
          (store-cand addr val ref)
          (store-spec addr val)))

    ))

(define (test1)
  (define mem (new memory-racket%))
  (send mem load 9) ;; expect error here
  (send mem load 6)
  (send mem store 2 222)
  (send mem store 9 999)
  (send mem load 9)
  (send mem store 9 0)
  (send mem load 9)
  (send mem print)

  (define mem2 (send mem clone mem))
  (send mem2 load 9)
  (send mem2 load 6)
  (send mem2 store 2 222)
  (send mem2 store 9 999)
  (send mem2 load 9)
  (send mem2 store 9 0)
  (send mem2 load 9)
  (send mem2 print))

(define (test2)
  ;; test correctness-cost
  (define (diff-cost x y) (if (= x y) 0 1))
    
  (define mem (new memory-racket% [init (make-hash '((9 . 99) (6 . 66)))]))
  (send mem load 9)
  (send mem load 6)
  (send mem store 2 222)
  (send mem store 3 333)

  (define mem2 (send mem clone mem))
  (send mem2 load 9)
  (send mem2 load 6)
  (send mem2 store 2 111)

  (assert (= (send mem correctness-cost mem2 diff-cost 10) 11))
  (pretty-display "test 2: passed")
  )

(define (test3)
  ;; test clone-all del lookup-update
  (define mem (new memory-racket% [init (make-hash '((9 . 99) (6 . 66)))]))
  (send mem store 2 222)
  (send mem store 3 333)

  (define mem2 (send mem clone-all))
  (send mem2 del 3)
  (assert (= 222 (send mem2 lookup-update 2)))
  (assert (equal? #f (send mem2 lookup-update 3)))
  (assert (= 333 (send mem lookup-update 3)))
  (pretty-display "test 3: passed")
  )

(define (test4)
  (define state1 (vector 1 (new memory-racket% [update (make-hash '((9 . 99) (6 . 66)))])))
  (define state2 (vector 1 (new memory-racket% [update (make-hash '((9 . 99) (6 . 66)))])))

  (assert (equal? state1 state2))
  (define my-hash (make-hash))
  (hash-set! my-hash state1 1234)
  (assert (= 1234 (hash-ref my-hash state2)))
  (pretty-display "test 4: passed")
  )

(define (test-all)
  (test2)
  (test3)
  (test4))
