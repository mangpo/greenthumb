#lang s-exp rosette

(require "special.rkt" "memory-racket.rkt" "ops-rosette.rkt")
(provide memory-rosette% init-memory-size increase-memory-size finalize-memory-size)

(define memory-size 1)
(define (init-memory-size)
  (set! memory-size 1))
(define (increase-memory-size)
  (when (> memory-size 100) (raise "memory-rosette: memory size is too large."))
  (pretty-display (format "Increase memory size to ~a" (* 2 memory-size)))
  (set! memory-size (* 2 memory-size)))
(define (finalize-memory-size)
  (set! memory-size (add1 memory-size))
  (pretty-display (format "Finalize memory size ~a" memory-size))
  )


;; Mask method which should only be public for object of memory-racket%
(define-local-member-name lookup-init)
(define-local-member-name lookup-update)

(define memory-rosette%
  (class* special% (equal<%> printable<%>)
    (super-new)
    (init-field get-fresh-val
                [size memory-size]
                [init (make-vector size)] ;; TODO: change to list
                [update (make-vector size)]
                ;; [get-fresh-val
                ;;  (lambda ()
                ;;    (define-symbolic* val number?)
                ;;    val)]
                ;; If this memory object is for interpreting specification program,
                ;; don't initialize ref.
                ;; Otherwise, initialize ref with memory object output from specification program.
                [ref #f])
    (public load store create-concrete clone)
    
    (define (cal-size l)
      (define ans 0)
      (for ([x l])
           (when (pair? x) (set! ans (add1 ans))))
      ans)

    (define/public (custom-print port depth)
      (print `(memory% init: ,init update: ,update size: ,(cal-size update)) port depth))

    (define/public (custom-write port)
      (write `(memory% init: ,init update: ,update size: ,(cal-size update)) port))

    (define/public (custom-display port)
      (display `(memory% init: ,init update: ,update size: ,(cal-size update)) port))

    (define/public (equal-to? other recur)
      (and (is-a? other memory-rosette%)
           (equal? update (get-field* update other))))

    (define/public (equal-hash-code-of hash-code)
      (hash-code update))

    (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code update))
      
    ;; Create concrete memory object by evaluating symbolic memory.
    (define (create-concrete eval)
      (new memory-racket% [init (make-hash (filter pair? (vector->list (eval init))))]))

    ;; Clone a new symbolic memory object with the same init.
    ;; Use this method to clone new memory for every program interpretation.
    (define (clone [ref #f])
      (new memory-rosette% [ref ref] [init init] [update (vector-copy update)]
           [get-fresh-val get-fresh-val]))

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
        (and (< index (vector-length storage))
             (let ([pair (vector-ref storage index)])
               (and (pair? pair)
                    (if (equal? addr (car pair))
                        (cdr pair)
                        (loop (add1 index)))
                    ))))
      (loop 0))

    
    (define/public (lookup-init addr) (lookup init addr))
    (define/public (lookup-update addr) (lookup update addr))

    (define (modify storage addr val)
      (define (loop index)
        (and (< index (vector-length storage))
             (let ([pair (vector-ref storage index)])
               (and (pair? pair)
                    (if (equal? addr (car pair))
                        (begin
                          (vector-set! storage index (cons addr val))
                          #t)
                        (loop (add1 index)))
                    ))))
      (loop 0))
    
    ;;;;;;;;;;;;;;;;;;;; load ;;;;;;;;;;;;;;;;;;;;
    
    (define (load-spec addr)
      (or (lookup update addr)
          (lookup init addr)
          (init-new-val addr)))

    (define (load-cand addr ref-mem)
      (or (lookup update addr)
          (send* ref-mem lookup-init addr) ;; TODO: (lookup init addr) should work too
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
        [(send* mem-ref lookup-update addr)
         (store-spec addr val)]
        [else (assert #f "store illegal address")]))
      
    
    (define (store addr val)
      (if ref
          (store-cand addr val ref)
          (store-spec addr val)))

    ))

(define (test)
  (set! memory-size 2)
  (define func (lambda () (define-symbolic* val number?) val))
  (define mem (new memory-rosette% [get-fresh-val func]))
  (assert (term? (send mem load 9)) "mem: load 9 = sym")
  (assert (term? (send mem load 6)) "mem: load 6 = sym")
  (send mem store 2 222)
  (assert (= (send mem load 2) 222) "mem: load 2 = 222")
  (send mem store 9 999)
  (assert (= (send mem load 9) 999) "mem: load 9 = 999")
  (send mem store 9 0)
  (assert (= (send mem load 9) 0) "mem: load 9 = 0")
  (pretty-display `(mem ,mem))
  
  (define mem2 (send mem clone mem))
  (assert (= (send mem2 load 9) 0) "mem2: load 9 = 0")
  (assert (term? (send mem2 load 6)) "mem2: load 6 = sym")
  (assert (= (send mem2 load 2) 222) "mem2: load 2 = 222")
  (pretty-display `(mem2 ,mem2)))
