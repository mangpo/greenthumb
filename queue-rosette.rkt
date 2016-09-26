#lang s-exp rosette

(require "queue-racket.rkt")
(provide queue-rosette%)

(define queue-rosette%
  (class* object% (printable<%>)
    (super-new)
    (init-field get-fresh-val
                [size 20]
                [queue (make-vector size #f)]
                [index 0]
                ;; [get-fresh-val
                ;;  (lambda ()
                ;;    (define-symbolic* val number?)
                ;;    val)]
                ;; If this memory object is for interpreting specification program,
                ;; don't initialize ref.
                ;; Otherwise, initailize ref with memory object output from specification program.
                [ref #f])

    (public push pop create-concrete clone)

    (define/public (custom-print port depth)
      (print `(queue% content: ,queue index: ,index) port depth))

    (define/public (custom-write port)
      (write `(queue% content: ,queue index: ,index) port))

    (define/public (custom-display port)
      (display `(queue% content: ,queue index: ,index) port))

    ;; Create concrete memory object by evaluating symbolic memory.
    (define (create-concrete eval)
      (new queue-racket% [queue (eval queue)] [index (eval index)]))               
    
    ;; Clone a new queue object with the same queue.
    ;; Use this method to clone new memory for every program interpretation.
    (define (clone [ref #f])
      (new queue-rosette% [ref ref] [queue queue] [index index]  [get-fresh-val get-fresh-val]))

    (define (pop-spec)
      (define val (get-fresh-val))
      (vector-set! queue index val)
      (set! index (add1 index))
      val)

    (define (pop-cand)
      (assert (< index (get-field index ref)) "pop-cand: pop more than spec does")
      (define val (vector-ref queue index))
      (set! index (add1 index))
      val)

    (define (pop)
      (if ref (pop-cand) (pop-spec)))
    
    (define (push-spec val)
      (vector-set! queue index val)
      (set! index (add1 index)))

    (define (push-cand val)
      (assert (< index (get-field index ref)) "push-cand: push more than spec does")
      (define val-ref (vector-ref queue index))
      (assert (equal? val val-ref) "push-cand: push a value different from spec does")
      (set! index (add1 index)))

    (define (push val) (if ref (push-cand val) (push-spec val)))
    
    ))

;; (define queue-out-rosette%
;;   (class* object% (printable<%>)
;;     (super-new)
;;     (init-field [size 20]
;;                 [queue (make-vector size)]
;;                 [index 0]
;;                 ;; If this memory object is for interpreting specification program,
;;                 ;; don't initialize ref.
;;                 ;; Otherwise, initailize ref with memory object output from specification program.
;;                 [ref #])

;;     (public create-concrete clone push)

;;     (define/public (custom-print port depth)
;;       (print `(queue-out% content: ,queue index: ,index) port depth))

;;     (define/public (custom-write port)
;;       (write `(queue-out% content: ,queue index: ,index) port))

;;     (define/public (custom-display port)
;;       (display `(queue-out% content: ,queue index: ,index) port))

;;     ;; Create concrete memory object by evaluating symbolic memory.
;;     (define (create-concrete eval)
;;       (new queue-out-racket% [queue (eval queue)] [index (eval index)]))               
    
;;     ;; Clone a new queue object with the same queue.
;;     ;; Use this method to clone new memory for every program interpretation.
;;     (define (clone [ref #f])
;;       (new queue-out-rosette% [ref ref] [queue queue] [index index]))
    
;;     (define (push-spec val)
;;       (vector-set! queue index val)
;;       (set! index (add1 index)))

;;     (define (push-cand val)
;;       (assert (< index (get-field index ref)))
;;       (define val-ref (vector-ref queue index))
;;       (assert (equal? val val-ref))
;;       (set! index (add1 index)))

;;     (define (push val) (if ref (push-cand val) (push-ref val)))

;;     ))

(define (test1)
  (define q-init (new queue-rosette%))
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
  (define q-init (new queue-rosette%))
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
