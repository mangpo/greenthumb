#lang s-exp rosette

(require "memory-racket.rkt")
(provide memory-rosette%)

(define memory-rosette%
  (class object%
    (super-new)
    (init-field [size 20]
                [init (make-vector size)]
                [update (make-vector size)]
                [get-fresh-val
                 (lambda ()
                   (define-symbolic* val number?)
                   val)]
                [ref #f]
                )
    ;; mode = `spec
    ;; mode = `candidate
    (public print load store create clone
            lookup-init lookup-update update-equal?)

    (define (print)
      (pretty-display (format "init: ~a" init))
      (pretty-display (format "update: ~a" update)))
      

    (define (create sol eval)
      (new memory-racket% [init (eval init)]))
      
    (define (clone [ref #f])
      (new memory-rosette% [ref ref] [init init]))

    (define (update-equal? other)
      (for/and ([pair update] #:break (not (pair? pair)))
               (let ([addr (car pair)]
                     [val (cdr pair)])
                 (= val (send other lookup-update addr)))))

    (define (init-new-val addr)
      (define (loop index)
        (if (pair? (vector-ref init index))
            (loop (add1 index))
            (let ([val (get-fresh-val)])
              (vector-set! init index (cons addr val))
              val)))
      (loop 0))

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
          (init-new-val addr)))

    (define (load-cand addr ref-mem)
      (or (lookup update addr)
          (send ref-mem lookup-init addr)
          (assert #f "load illegal address")))

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

#|
(define mem (new memory-rosette%))
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
(send mem2 print)
|#
    
