#lang s-exp rosette

(require racket/list "stack.rkt")

(provide (all-defined-out))

(struct progstate (a b p i r s t data return memory recv comm cost) 
        #:mutable #:transparent)

;;; The empty constraint. Pretty useless.
(define constraint-none (progstate #f #f #f #f #f #f #f #f #f #f #f #t #t))

;;; Constrain everything. We must have perfection!
(define constraint-all (progstate #t #t #t #t #t #t #t 8 8 #t #f #t #t))

;;; Defines a constraint for some fields. For example, `(constraint
;;; t)' is the same as constraint-only-t and evaluates to `(progstate
;;; #f #f #f #f #f #f #t #f #f #f)'. If you want to constrain
;;; everything *except* the given fields, start with the except
;;; keyword: `(constrain except t)' constrains everything but t. 
(define-syntax constraint
  (syntax-rules (except data return)

    ((constraint (return val1) (data val2) var ...)  
     (struct-copy progstate constraint-none [return val1] [data val2] [var #t] ...))
    ((constraint (data val2) (return val1) var ...)  
     (struct-copy progstate constraint-none [return val1] [data val2] [var #t] ...))

    ((constraint (return val1) (data val2))
     (struct-copy progstate constraint-none [return val1] [data val2]))
    ((constraint (data val2) (return val1))
     (struct-copy progstate constraint-none [return val1] [data val2]))

    ((constraint (data val) var ...)  
     (struct-copy progstate constraint-none [data val] [var #t] ...))
    ((constraint (data val))  
     (struct-copy progstate constraint-none [data val]))
    ((constraint (return val) var ...)  
     (struct-copy progstate constraint-none [return val] [var #t] ...))
    ((constraint (return val))  
     (struct-copy progstate constraint-none [return val]))

    ((constraint except var ...) (struct-copy progstate constraint-all  [var #f] ...))
    ((constraint var ...)        (struct-copy progstate constraint-none [var #t] ...))
    ))

(define (sym-input)
  (define-symbolic* input number?)
  input)

(define (get-sym-vars state)
  (define lst (list))
  (define (add x)
    (when (sym? x)
          (set! lst (cons x lst))))

  (define (add-var progstate-x)
    (add (progstate-x state)))

  (define (add-vector body)
    (for ([i (in-range (vector-length body))])
         (add (vector-ref body i))))

  (add-var progstate-a)
  (add-var progstate-b)
  (add-var progstate-p)
  (add-var progstate-i)
  (add-var progstate-r)
  (add-var progstate-s)
  (add-var progstate-t)
  (add (stack-sp (progstate-data state)))
  (add (stack-sp (progstate-return state)))
  (add-vector (stack-body (progstate-data state)))
  (add-vector (stack-body (progstate-return state)))
  (add-vector (progstate-memory state))
  (for ([x (progstate-recv state)])
       (add x))

  lst)

(define-syntax default-state
  (syntax-rules (data-pair concrete)
    ((default-state info init) 
     (match info
      [(cons mem recv-n)
       (progstate init init 0 0
                  init init init
                  (stack 0 (list->vector (for/list ([i (in-range 8)]) init))) ;; data
                  (stack 0 (list->vector (for/list ([i (in-range 8)]) init))) ;; return
                  (list->vector (for/list ([i (in-range mem)]) init))  ;; memory
                  (for/list ([i (in-range recv-n)]) init) ;; recv
                  (list) ;; comm
                  0 ;; cost
                  )]))

    ((default-state info init [data-pair (i i-val) ...] [key val] ...)
     (let* ([state (default-state info init)]
            [body (stack-body (progstate-data state))]
            [pairs (list (cons i i-val) ...)])
       (for ([p pairs])
            (vector-set! body (modulo (- (car p)) 8) (cdr p)))
       (struct-copy progstate state [data (stack 0 body)] [key val] ...)))

    ((default-state info init [key val] ...)
     (struct-copy progstate (default-state info init) [key val] ...))
    
    ((default-state)
     (default-state (cons 0 0) 0))
    ))

(define (constrain-stack precond)
  (define state (default-state))
  (when (list? precond)
    (for ([assume precond]
          [i (reverse (range (length precond)))])
      (cond
       [(and assume (= i 0)) (set-progstate-t! state assume)]
       [(and assume (= i 1)) (set-progstate-s! state assume)]
       [(and assume (< i 10))
        (let ([body (stack-body (progstate-data state))])
          (vector-set! body (modulo (- 2 i) 8) assume))]

       [(and assume (<= i 10))
	(raise "A small function cannot have more than 10 parameters.")]
       )))
  state)

;; (define (add-stack-cnstr cnstr extra)
;;   (if (= extra 0) 
;;       cnstr
;;       (let ([new-n (+ extra
;;                       (cond
;;                        [(> 0 (progstate-data cnstr)) (+ (progstate-data cnstr) 2)]
;;                        [(progstate-s cnstr) 2]
;;                        [(progstate-t cnstr) 1]
;;                        [else 0]))])
;;         (cond
;;          [(= new-n 0) cnstr]
;;          [(= new-n 1) (struct-copy progstate cnstr [t #t])]
;;          [(= new-n 2) (struct-copy progstate cnstr [s #t] [t #t])]
;;          [else (struct-copy progstate cnstr [s #t] [t #t] [data (- new-n 2)])]))))

(define (create-constraint lst extra-data extra-return)
  (define a #f) 
  (define b #f) 
  (define memory #f)
  (define data extra-data)
  (define return extra-return)
  (for ([i lst])
       (cond
        [(equal? i 'a)      (set! a #t)]
        [(equal? i 'b)      (set! b #t)]
        [(equal? i 'memory) (set! memory #t)]
        [(and (pair? i) (equal? (car i) 'data))   (set! data (+ data (cdr i)))]
        [(and (pair? i) (equal? (car i) 'return)) (set! return (+ return (cdr i)))]
        [else (raise (format "create-constraint: unimplemented for ~a" i))]))
  (struct-copy progstate constraint-none [a a] [b b] [memory memory]
               [t (>= data 1)] [s (>= data 2)] [data (max 0 (- data 2))]
               [r (>= return 1)] [return (max 0 (- return 1))]))

(define (progstate-copy x)
  (struct-copy progstate x
               [data (stack (stack-sp (progstate-data x))
                            (vector-copy (stack-body (progstate-data x))))]
               [return (stack (stack-sp (progstate-return x))
                              (vector-copy (stack-body (progstate-return x))))]
               [memory (vector-copy (progstate-memory x))]))

;;; Print the data stack:
(define (display-data state)
  (display (format "|d> ~a ~a" (progstate-t state) (progstate-s state)))
  (display-stack (progstate-data state))
  (newline))

;;; Print the return stack:
(define (display-return state)
  (display (format "|r> ~a" (progstate-r state)))
  (display-stack (progstate-return state))
  (newline))

;;; Print the memory:
(define (display-memory state)
  (define memory (progstate-memory state))
  (display "mem: ")
  (for ([i (in-range 0 (vector-length memory))])
    (display (format "~a " (vector-ref memory i))))
  (newline))

;;; Print comm info.
(define (display-comm state)
  (display "recv: ")
  (pretty-display (progstate-recv state))
  (display "comm: ")
  (pretty-display (progstate-comm state)))

;;; Displays some state, useful for debugging. Currently this just
;;; shows the pc and data stack.
(define (display-state state)
  (pretty-display (format "a:~a b:~a"
                          (progstate-a state) (progstate-b state)))
  (display-data state)
  (display-return state)
  (display-memory state)
  (display-comm state)
  (pretty-display (format "cost: ~a" (progstate-cost state)))
  )