#lang racket

(require "ops-racket.rkt")
(provide queue-racket%)

(define queue-racket%
  (class* object% (equal<%> printable<%>)
    (super-new)
    (init-field [size 20]
                [queue (make-vector size)]
                [index 0]
                [ref #f])

    (public push pop push-inverse pop-inverse
            clone correctness-cost)

    (define/public (custom-print port depth)
      (print `(queue% content: ,(vector-copy queue 0 index)) port depth))

    (define/public (custom-write port)
      (write `(queue% content: ,(vector-copy queue 0 index)) port))

    (define/public (custom-display port)
      (display `(queue% content: ,(vector-copy queue 0 index)) port))

    (define/public (equal-to? other recur)
      (equal? index (get-field index other)))

    (define/public (equal-hash-code-of hash-code)
      (hash-code index))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code index))

    (define (clone [ref #f])
      (new queue-racket% [ref ref] [queue queue] [index index]))

    (define (pop)
      (define val (vector-ref queue index))
      (assert (not (equal? val #f)) "pop more than number of elements")
      (set! index (add1 index))
      val)

    (define (pop-inverse val)
      (set! index (sub1 index))
      (define val-ref (vector-ref queue index))
      (assert (equal? val val-ref) "pop-inverse: different val"))

    (define (push val)
      (define val-ref (vector-ref queue index))
      (assert (equal? val val-ref) "push different val")
      (set! index (add1 index)))

    (define (push-inverse)
      (set! index (sub1 index))
      (vector-ref queue index))
    
    (define (correctness-cost other diff-cost bit)
      (* bit (abs (- (get-field index other) index))))
    
    ))

;; (define queue-out-racket%
;;   (class* object% (equal<%> printable<%>)
;;     (super-new)
;;     (init-field [size 20]
;;                 [queue (make-vector size)]
;;                 [index 0]
;;                 [ref #f])

;;     (public push clone correctness-cost)

;;     (define/public (custom-print port depth)
;;       (print `(queue-out% content: ,queue index: ,index) port depth))

;;     (define/public (custom-write port)
;;       (write `(queue-out% content: ,queue index: ,index) port))

;;     (define/public (custom-display port)
;;       (display `(queue-out% content: ,queue index: ,index) port))
    
;;     (define/public (equal-to? other recur)
;;       (equal? index (get-field index other)))

;;     (define/public (equal-hash-code-of hash-code)
;;       (hash-code index))

;;     (define/public (equal-secondary-hash-code-of hash-code)
;;       (hash-code index))
    
;;     (define (clone [ref #f])
;;       (new queue-out-racket% [ref ref] [queue queue] [index index]))

;;     (define (push val)
;;       (define val-ref (vector-ref queue index))
;;       (assert (not (equal? val-ref #f)))
;;       (assert (equal? val val-ref))
;;       (set! index (add1 index)))

;;     (define (correctness-cost other diff-cost bit)
;;       (* bit (abs (- (get-field index other) index))))

;;     ))

(define (test1)
  (define q-init (new queue-racket% [queue (vector 1 2 #f #f)] [index 0]))

  (define q1 (send q-init clone))
  (pretty-display `(pop ,(send q1 pop)))
  (pretty-display `(pop ,(send q1 pop)))
  (pretty-display `(queue-init ,q1))
  (pretty-display `(pop ,(send q1 pop))) ;; assert: pop-cand: pop more than spec does
  )

(define (test2)
  (define q-init (new queue-racket% [queue (vector 1 2 #f #f)] [index 0]))

  (define q2 (send q-init clone))
  (send q2 push 1)
  (pretty-display `(queue ,q2))
  (send q2 push 22) ;; assert: push-cand: push a value different from spec does
  )
      
