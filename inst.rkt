#lang racket

(provide (all-defined-out))

(struct inst (op args) #:mutable)

(define-syntax-rule (timeout sec expr)
  (let* ([t (let ([parent (current-thread)])
              (thread
               (thunk
                (thread-send 
                 parent
                 (with-handlers [(exn? identity)]
                                expr)))))]
         [out (sync/timeout sec t)])
    (cond [out  (thread-receive)]
          [else (break-thread t)
                (raise (thread-receive))])))

(define (random-from-vec vec)
  (vector-ref vec (random (vector-length vec))))

(define (random-from-list lst)
  (list-ref lst (random (length lst))))

(define (random-from-list-ex lst ex)
  (let ([new-lst (remove ex lst)])
    (if (empty? new-lst)
        ex
        (list-ref new-lst (random (length new-lst))))))

(define (random-from-vec-ex vec ex)
  (define len (vector-length vec))
  (define (inner)
    (define sample (vector-ref vec (random len)))
    (if (eq? sample ex)
        (inner)
        sample))
  (if (= len 1)
      (vector-ref vec 0)
      (inner)))


(struct pointer (type))
(define-syntax-rule (pointer-type? p t)
  (and (pointer? p) (equal? (pointer-type p) t)))
