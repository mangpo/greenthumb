#lang racket

(require "special.rkt" "ops-racket.rkt")
(provide queue-in-racket% queue-out-racket%)

(define queue-in-racket%
  (class* special% (equal<%> printable<%>)
    (super-new)
    (init-field [get-fresh-val #f]
                [size 4]
                [queue (make-vector size #f)]
                [index 0]
                [ref #f])

    (public pop pop-inverse
            clone correctness-cost get-live-mask)
    
    (define (filter-queue)
      (filter (lambda (x) (not (equal? x #f))) (vector->list queue)))

    (define/public (relevant-queue) (vector-copy queue 0 index))
    
    (define/public (custom-print port depth)
      (print `(queue-in% content: ,(filter-queue) index: ,index) port depth))

    (define/public (custom-write port)
      (write `(queue-in% content: ,(filter-queue) index: ,index) port))

    (define/public (custom-display port)
      (display `(queue-in% content: ,(filter-queue) index: ,index) port))
    
    (define/public (equal-to? other recur)
      (and (is-a? other queue-in-racket%)
           (equal? (relevant-queue) (send other relevant-queue))
           ))

    (define/public (equal-hash-code-of hash-code)
      (hash-code (relevant-queue)))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (relevant-queue)))
    
    ;; (define/public (equal-to? other recur)
    ;;   (and (is-a? other queue-in-racket%)
    ;;        (equal? index (get-field index other))
    ;;        ))

    ;; (define/public (equal-hash-code-of hash-code)
    ;;   (hash-code index))

    ;; (define/public (equal-secondary-hash-code-of hash-code)
    ;;   (hash-code index))

    (define (clone [ref #f])
      (new queue-in-racket% [ref ref] [queue queue] [index index] [get-fresh-val get-fresh-val]))

    ;; (define (clone-all [ref #f])
    ;;   (new queue-in-racket% [ref ref] [queue queue] [index index]))

    (define (pop-spec)
      (define val (vector-ref queue index))
      (unless val
              (set! val (get-fresh-val))
              (vector-set! queue index val))
      (set! index (add1 index))
      val)
    
    (define (pop-cand)
      (define val (vector-ref queue index))
      (assert (not (equal? val #f)) "pop more than number of elements")
      (set! index (add1 index))
      val)

    (define (pop)
      (if ref (pop-cand) (pop-spec)))

    (define (pop-inverse val)
      (set! index (sub1 index))
      (define val-ref (vector-ref queue index))
      (equal? val val-ref))
    
    (define (correctness-cost other diff-cost bit)
      (* bit (abs (- (get-field index other) index))))

    (define (get-live-mask) #t)
    
    ))

(define queue-out-racket%
  (class* special% (equal<%> printable<%>)
    (super-new)
    (init-field [get-fresh-val #f]
                [size 4]
                [queue (make-vector size)]
                [index 0]
                [ref #f])

    (public push push-inverse
            clone correctness-cost get-live-mask)

    (define/public (get-at index) (vector-ref queue index))
    
    (define (filter-queue)
      (filter (lambda (x) (not (equal? x #f))) (vector->list queue)))

    (define/public (relevant-queue) (vector-copy queue 0 index))

    (define/public (custom-print port depth)
      (print `(queue-out% content: ,(filter-queue) index: ,index) port depth))

    (define/public (custom-write port)
      (write `(queue-out% content: ,(filter-queue) index: ,index) port))

    (define/public (custom-display port)
      (display `(queue-out% content: ,(filter-queue) index: ,index) port))

    (define/public (equal-to? other recur)
      (and (is-a? other queue-out-racket%)
           (equal? (relevant-queue) (send other relevant-queue))
           ))

    (define/public (equal-hash-code-of hash-code)
      (hash-code (relevant-queue)))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code (relevant-queue)))
    
    ;; (define/public (equal-to? other recur)
    ;;   (and (is-a? other queue-in-racket%)
    ;;        (equal? index (get-field index other))
    ;;        ))

    ;; (define/public (equal-hash-code-of hash-code)
    ;;   (hash-code index))

    ;; (define/public (equal-secondary-hash-code-of hash-code)
    ;;   (hash-code index))

    (define (clone [ref #f])
      (new queue-out-racket% [ref ref] [queue (vector-copy queue)] [index index]
           [get-fresh-val get-fresh-val]))

    (define (push-spec val)
      (vector-set! queue index val)
      (set! index (add1 index)))

    (define (push-cand val)
      (define val-ref (send ref get-at index))
      (assert (equal? val val-ref) "push-cand: push different val")
      (vector-set! queue index val)
      (set! index (add1 index)))

    (define (push val)
      (if ref (push-cand val) (push-spec val)))

    (define (push-inverse)
      (set! index (sub1 index))
      (vector-ref queue index))
    
    (define (correctness-cost other diff-cost bit)
      (* bit (abs (- (get-field index other) index))))
    
    (define (get-live-mask) #t)
    
    ))


(define (test1)
  (define q-init (new queue-in-racket% [queue (vector 1 2 #f #f)] [index 0]))

  (define q1 (send q-init clone))
  (pretty-display `(pop ,(send q1 pop)))
  (pretty-display `(pop ,(send q1 pop)))
  (pretty-display `(queue-init ,q1))
  (pretty-display `(pop ,(send q1 pop))) ;; assert: pop-cand: pop more than spec does
  )

(define (test2)
  (define q-init (new queue-out-racket% [queue (vector 1 2 #f #f)] [index 0]))

  (define q2 (send q-init clone))
  (send q2 push 1)
  (pretty-display `(queue ,q2))
  (send q2 push 22) ;; assert: push-cand: push a value different from spec does
  )

(define (test-performance)
  (define q (new queue-out-racket%))
  (for/list ([i 1000000])
    (send q clone)))

      
