#lang racket

(require "../machine.rkt")

(provide GA-machine% (all-defined-out))

(struct syninfo   (memsize recv indexmap))
(struct blockinfo (cnstr recv))
(struct labelinfo (data return simple))

(struct progstate (a b r s t data return memory recv comm) 
        #:mutable #:transparent)

(define UP #x145) ;325
(define DOWN #x115) ;277
(define LEFT #x175) ;373
(define RIGHT #x1d5) ;469
(define IO #x15d)


;;;;;;;;;;;;;; STACK ;;;;;;;;;;;;;;;;;;
(struct stack (sp body))

;;; Print a circular stack:
(define (display-stack stack)
  (define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
  ;; (pretty-display (format " ~a" stack)))
  (for [(i (in-range 0 8))]
       (display (format " ~a" (vector-ref (stack-body stack)
                                          (modulo- (- (stack-sp stack) i) 8))))))

;;;;;;;;;;;;;; END STACK ;;;;;;;;;;;;;;;;

(define-syntax-rule (build-vector n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i (init)))
    vec))

(define-syntax default-state
  (syntax-rules ()
    ((default-state machine recv-n init)
     (progstate (init) (init) (init) (init) (init)
		(stack 0 (build-vector 8 init))
		(stack 0 (build-vector 8 init))
		(build-vector (send machine get-nmems) init)
		(for/list ([i recv-n]) (init))
		(list)))

    ((default-state machine recv-n init [data-pair (i i-val) ...] [key val] ...)
     (let* ([state (default-state machine recv-n init)]
            [body (stack-body (progstate-data state))]
            [pairs (list (cons i i-val) ...)])
       (for ([p pairs])
            (vector-set! body (modulo (- (car p)) 8) (cdr p)))
       (struct-copy progstate state [data (stack 0 body)] [key val] ...)))

    ((default-state machine recv-n init [key val] ...)
     (struct-copy progstate (default-state machine recv-n init) [key val] ...))
    
    ((default-state machine)
     (default-state machine 0 (lambda () 0)))
    ))

;;; The empty constraint. Pretty useless.
(define constraint-none (progstate #f #f #f #f #f #f #f #f #f #t))

;;; Constrain everything. We must have perfection!
(define constraint-all (progstate #t #t #t #t #t 8 8 #t #f #t))

;;; Defines a constraint for some fields. For example, `(constraint
;;; t)' is the same as constraint-only-t and evaluates to `(progstate
;;; #f #f #f #f #f #f #t #f #f #f)'. If you want to constrain
;;; everything *except* the given fields, start with the except
;;; keyword: `(constrain except t)' constrains everything but t. 
(define-syntax constraint
  (syntax-rules (except data return none)

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

(define (constrain-stack machine precond [state (default-state machine)])
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

(define GA-machine%
  (class machine%
    (super-new)
    (inherit-field bit random-input-bit inst-id classes classes-len perline)
    (inherit print-line)
    (override set-config get-config set-config-string
              adjust-config config-exceed-limit?
              get-state display-state
              output-constraint-string output-assume-string
	      no-assumption)

    (set! bit 18)
    (set! random-input-bit 16)
    (set! inst-id '#(nop @p @+ @b @ !+ !b ! +* 2* 
			 2/ - + and or drop dup pop over a 
			 push b! a!))

    ;; Instruction classes
    (set! classes 
          (vector '(@+ @b @) 
		  '(!+ !b !)
		  '(+* 2* 2/ -)
		  '(+ and or drop push b! a!) 
		  '(dup pop over a)))

    (set! classes-len (vector-length classes))
    (set! perline 8)

    (define nmems 1)
    (define/public (get-nmems) nmems)

    (define (get-config) nmems)
    (define (set-config info) (set! nmems info))
    (define (set-config-string info) info)
    (define (adjust-config info) (* 2 info))
    (define (config-exceed-limit? info) (> info 100))
    (define (output-assume-string machine-var x)
      (format "(constrain-stack ~a '~a)" machine-var x))
    (define (output-constraint-string machine-var live-out)
      ;; live-out is something like '((data . 0) (return . 1) memory a)
      (format "(send ~a output-constraint '~a)" machine-var live-out))

    (define/public (output-constraint lst [extra-data 0] [extra-return 0])
      (define a #f) 
      (define b #f) 
      (define memory (make-vector nmems #f))
      (define data extra-data)
      (define return extra-return)
      (for ([i lst])
	   (cond
	    [(equal? i 'a)      (set! a #t)]
	    [(equal? i 'b)      (set! b #t)]
	    [(equal? i 'memory) (set! memory (make-vector nmems #t))]
	    [(and (pair? i) (equal? (car i) 'data))   (set! data (+ data (cdr i)))]
	    [(and (pair? i) (equal? (car i) 'return)) (set! return (+ return (cdr i)))]
	    [else (raise (format "create-constraint: unimplemented for ~a" i))]))
      (struct-copy progstate constraint-none [a a] [b b] [memory memory]
		   [t (>= data 1)] [s (>= data 2)] [data (and (> (- data 2) 0) (- data 2))]
		   [r (>= return 1)] [return (and (> (- return 1) 0) (- return 1))]))

    (define (get-state init recv-n) ;; TODO: track all get-state
      (default-state this recv-n init))
      
    (define (display-state state)
      (pretty-display (format "a:~a b:~a"
			      (progstate-a state) (progstate-b state)))
      (display-data state)
      (display-return state)
      (display-memory state)
      (display-comm state)
      )

    ;; Print the data stack:
    (define (display-data state)
      (display (format "|d> ~a ~a" (progstate-t state) (progstate-s state)))
      (display-stack (progstate-data state))
      (newline))

    ;; Print the return stack:
    (define (display-return state)
      (display (format "|r> ~a" (progstate-r state)))
      (display-stack (progstate-return state))
      (newline))

    ;; Print the memory:
    (define (display-memory state)
      (define memory (progstate-memory state))
      (display "mem: ")
      (for ([i (in-range 0 (vector-length memory))])
	   (display (format "~a " (vector-ref memory i))))
      (newline))

    ;; Print comm info.
    (define (display-comm state)
      (display "recv: ")
      (pretty-display (progstate-recv state))
      (display "comm: ")
      (pretty-display (progstate-comm state)))

    (define (no-assumption)
      (default-state this))
    ))
