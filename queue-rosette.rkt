#lang s-exp rosette

(require "queue-racket.rkt" "special.rkt" "ops-rosette.rkt")
(provide queue-in-rosette% queue-out-rosette%)

(define queue-in-rosette%
  (class* special% (equal<%> printable<%>)
    (super-new)
    (init-field get-fresh-val
                [size 4]
                [init (make-vector size #f)]
                [queue (make-vector size #f)]
                [ref #f])

    (public pop create-concrete clone)

    (define/public (custom-print port depth)
      (print `(queue-in-rosette% content: ,init ,queue) port depth))

    (define/public (custom-write port)
      (write `(queue-in-rosette% content: ,init ,queue) port))

    (define/public (custom-display port)
      (display `(queue-in-rosette% content: ,init ,queue) port))

    (define/public (equal-to? other recur)
      (and (is-a? other queue-in-rosette%)
           (equal? queue (get-field* queue other))))
      
    (define/public (equal-hash-code-of hash-code)
      (hash-code queue))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code queue))

    (define (get-size [index 0])
      (if (vector-ref queue index)
          (get-size (add1 index))
          index))
    
    ;; Create concrete memory object by evaluating symbolic memory.
    (define (create-concrete eval)
      (new queue-in-racket% [queue (eval init)]))        
    
    ;; Clone a new queue object with the same queue.
    ;; Use this method to clone new memory for every program interpretation.
    (define (clone [ref #f])
      (new queue-in-rosette% [ref ref] [init init] [queue (vector-copy queue)]
           [get-fresh-val get-fresh-val]))

    (define (pop-spec)
      (define size (get-size))
      (define val (vector-ref init size))
      (unless val
              (set! val (get-fresh-val))
              (vector-set! init size val))
      (vector-set! queue size val)
      val)

    (define (pop-cand)
      (define size (get-size))
      (define val (vector-ref init size))
      (vector-set! queue size val)
      val)

    (define (pop)
      (if ref (pop-cand) (pop-spec)))
    
    ))

(define queue-out-rosette%
  (class* special% (equal<%> printable<%>)
    (super-new)
    (init-field get-fresh-val
                [ref #f]
                [size 4]
                [queue (make-vector size #f)])

    (public push create-concrete clone)

    (define/public (custom-print port depth)
      (print `(queue-out-rosette% content: ,queue size: ,(get-size)) port depth))

    (define/public (custom-write port)
      (write `(queue-out-rosette% content: ,queue size: ,(get-size)) port))

    (define/public (custom-display port)
      (display `(queue-out-rosette% content: ,queue size: ,(get-size)) port))

    (define/public (equal-to? other recur)
      ;; (pretty-display `(equal-to? ,queue ,(get-field queue other)
      ;;                             ,(and (is-a? other queue-out-rosette%)
      ;;                                   (equal? queue (get-field queue other)))))
      (and (is-a? other queue-out-rosette%)
           (equal? queue (get-field* queue other))))
      
    (define/public (equal-hash-code-of hash-code)
      (hash-code queue))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code queue))

    (define (get-size [index 0])
      (if (vector-ref queue index)
          (get-size (add1 index))
          index))

    (define/public (get-at index) (vector-ref queue index))

    ;; Create concrete memory object by evaluating symbolic memory.
    (define (create-concrete eval)
      (new queue-out-racket% [queue (eval queue)]))               
    
    ;; Clone a new queue object with the same queue.
    ;; Use this method to clone new memory for every program interpretation.
    (define (clone [ref #f])
      (new queue-out-rosette% [ref ref] [queue (vector-copy queue)] [get-fresh-val get-fresh-val]))
    
    (define (push-spec val)
      (vector-set! queue (get-size) val))

    (define (push-cand val)
      (define size (get-size))
      (define val-ref (send* ref get-at size))
      (assert (equal? val val-ref) "push-cand: push a value different from spec does")
      (vector-set! queue size val))

    (define (push val)
      (if ref (push-cand val) (push-spec val)))
    
    ))

(define (test1)
  (define func (lambda () (define-symbolic* val number?) val))
  (define q-init (new queue-in-rosette% [get-fresh-val func]))
  (define q (send q-init clone))
  (pretty-display `(pop ,(send q pop)))
  (pretty-display `(pop ,(send q pop)))
  (pretty-display `(queue-init ,q-init))
  (pretty-display `(queue ,q))

  (define q1 (send q-init clone q))
  (pretty-display `(pop ,(send q1 pop)))
  (pretty-display `(queue-init ,q1))
  (pretty-display `(pop ,(send q1 pop)))
  (pretty-display `(pop ,(send q1 pop))) ;; assert: pop-cand: pop more than spec does
  )

(define (test2)
  (define func (lambda () (define-symbolic* val number?) val))
  (define q-init (new queue-out-rosette% [get-fresh-val func]))
  (define q (send q-init clone))
  (send q push 1)
  (send q push 2)
  (pretty-display `(queue-init ,q-init))
  (pretty-display `(queue ,q))

  (define q2 (send q-init clone q))
  (send q2 push 1)
  (pretty-display `(queue ,q2))
  (send q2 push 22) ;; assert: push-cand: push a value different from spec does
  )
