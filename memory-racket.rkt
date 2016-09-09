#lang racket

(require "ops-racket.rkt")
(provide memory-racket%)

(define memory-racket%
  (class object%
    (super-new)
    (init-field [size 20]
                [init (make-vector size)]
                [update (make-vector size)]
                ;; If this memory object is for interpreting specification program,
                ;; don't initialize ref.
                ;; Otherwise, initailize ref with memory object output from specification program.
                [ref #f])
    (public print load store clone update-equal? correctness-cost
            ;; internal use only
            lookup-init lookup-update 
            )

    (define (print)
      (pretty-display (format "init: ~a" init))
      (pretty-display (format "update: ~a" update)))
      
    ;; Clone a new symbolic memory object with the same init.
    ;; Use this method to clone new memory for every program interpretation.
    (define (clone [ref #f])
      (new memory-racket% [ref ref] [init init]))

    (define (update-equal? other)
      (for/and ([pair update] #:break (not (pair? pair)))
               (let* ([addr (car pair)]
                      [val (cdr pair)]
                      [other-val (send other lookup-update addr)])
                 (if other-val
                     (= val other-val)
                     (= val (send other lookup-init addr)))
                 )))

    (define (correctness-cost other diff-cost bit)
      (define other-all-updates
        (for/list ([pair (get-field update other)] #:break (not (pair? pair)))
                  (cdr pair)))
      (define cost 0)
      (for ([pair update] #:break (not (pair? pair)))
           (let* ([addr (car pair)]
                  [val (cdr pair)]
                  [other-val (or (send other lookup-update addr)
                                 (send other lookup-init addr))])
             (set! cost (+ cost
                           (if other-val
                               (diff-cost val other-val)
                               bit)))))
      cost)

    (define (update-new-loc addr val)
      (define (loop index)
        (if (pair? (vector-ref update index))
            (loop (add1 index))
            (vector-set! update index (cons addr val))))
      (loop 0))

    ;;;;;;;;;;;;;;;;;;;; lookup & update ;;;;;;;;;;;;;;;;;;;;
    (define (lookup storage addr)
      (define (loop index)
        (let ([pair (vector-ref storage index)])
          (and (pair? pair)
               (if (equal? addr (car pair))
                   (cdr pair)
                   (loop (add1 index)))
               )))
      (loop 0))

    
    (define (lookup-init addr) (lookup init addr))
    (define (lookup-update addr) (lookup update addr))

    (define (modify storage addr val)
      (define (loop index)
        (let ([pair (vector-ref storage index)])
          (and (pair? pair)
               (if (equal? addr (car pair))
                   (begin
                     (vector-set! storage index (cons addr val))
                     #t)
                   (loop (add1 index)))
               )))
      (loop 0))
    
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
      (or (modify update addr val)
          (update-new-loc addr val)))

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
  (send mem load 9)
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
  (define (diff-cost x y) (if (= x y) 0 1))
    
  (define mem (new memory-racket% [init #((9 . 99) (6 . 66) 0 0 0)]))
  (send mem load 9)
  (send mem load 6)
  (send mem store 2 222)
  (send mem store 3 333)

  (define mem2 (send mem clone mem))
  (send mem2 load 9)
  (send mem2 load 6)
  (send mem2 store 2 111)

  (= (send mem correctness-cost mem2 diff-cost 10) 11))
    
