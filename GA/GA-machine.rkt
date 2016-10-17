#lang racket

(require "../inst.rkt" "../machine.rkt" "../ops-racket.rkt"
         "../special.rkt" "../memory-racket.rkt" "../queue-racket.rkt")

(provide GA-machine% (all-defined-out))

;; (struct syninfo   (memsize recv indexmap))
;; (struct blockinfo (cnstr recv))
;; (struct labelinfo (data return simple))

;;;;;;;;;;;;;; PROGSTATE ;;;;;;;;;;;;;;;;;;
(define-syntax-rule
  (progstate a b r s t data return memory recv comm)
  (vector a b r s t data return memory recv comm))

(define-syntax-rule (progstate-a x) (vector-ref x 0))
(define-syntax-rule (progstate-b x) (vector-ref x 1))
(define-syntax-rule (progstate-r x) (vector-ref x 2))
(define-syntax-rule (progstate-s x) (vector-ref x 3))
(define-syntax-rule (progstate-t x) (vector-ref x 4))
(define-syntax-rule (progstate-data x)   (vector-ref x 5))
(define-syntax-rule (progstate-return x) (vector-ref x 6))
(define-syntax-rule (progstate-memory x) (vector-ref x 7))
(define-syntax-rule (progstate-recv x)   (vector-ref x 8))
(define-syntax-rule (progstate-comm x)   (vector-ref x 9))

(define-syntax-rule (set-progstate-a! x v) (vector-set! x 0 v))
(define-syntax-rule (set-progstate-b! x v) (vector-set! x 1 v))
(define-syntax-rule (set-progstate-r! x v) (vector-set! x 2 v))
(define-syntax-rule (set-progstate-s! x v) (vector-set! x 3 v))
(define-syntax-rule (set-progstate-t! x v) (vector-set! x 4 v))
(define-syntax-rule (set-progstate-data! x v)   (vector-set! x 5 v))
(define-syntax-rule (set-progstate-return! x v) (vector-set! x 6 v))
(define-syntax-rule (set-progstate-memory! x v) (vector-set! x 7 v))
(define-syntax-rule (set-progstate-recv! x v)   (vector-set! x 8 v))
(define-syntax-rule (set-progstate-comm! x v)   (vector-set! x 9 v))

;;;;;;;;;;;;;; STACK ;;;;;;;;;;;;;;;;;;
(define-syntax-rule (stack sp body) (vector sp body))
(define-syntax-rule (stack? x)
  (and (vector? x) (= 2 (vector-length x))
       (or (number? (vector-ref x 0)) (boolean? (vector-ref x 0)))
       (vector? (vector-ref x 1))))

(define-syntax-rule (stack-sp x) (vector-ref x 0))
(define-syntax-rule (stack-body x) (vector-ref x 1))
(define-syntax-rule (set-stack-sp! x v) (vector-set! x 0 v))
(define-syntax-rule (set-stack-body! x v) (vector-set! x 1 v))

;;; Print a circular stack:
(define (display-stack x)
  (display (format " ~a" x)))

(define-syntax-rule (build-vector n init)
  (let ([vec (make-vector n)])
    (for ([i (in-range n)])
	 (vector-set! vec i (init)))
    vec))

;;; The empty constraint. Pretty useless.
(define constraint-none (progstate #f #f #f #f #f 0 0 #f #f #t))

;;; Constrain everything. We must have perfection!
(define constraint-all (progstate #t #t #t #t #t 8 8 #t #f #t))

(define GA-machine%
  (class machine%
    (super-new)
    (inherit-field bitwidth random-input-bits config opcodes)
    (override display-state 
              parse-state-text
              progstate->vector vector->progstate
              get-constructor progstate-structure
              )
    (init-field [const-range #f]
                [UP #x145] ;325
                [DOWN #x115] ;277
                [LEFT #x175] ;373
                [RIGHT #x1d5] ;469
                [IO #x15d])
    (inherit init-machine-description define-instruction-class
             define-progstate-type define-arg-type
             finalize-machine-description get-state)

    (public stack->vector)

    (define (get-constructor) GA-machine%)

    (unless bitwidth (set! bitwidth 18))
    (unless random-input-bits
	    (if (= bitwidth 18)
		(set! random-input-bits 16)
		(set! random-input-bits (sub1 bitwidth))))

    (when (= bitwidth 4)
          (set! UP -2)
          (set! DOWN -4)
          (set! LEFT -6)
          (set! RIGHT -8)
          (set! IO 6))
    ;; (when (= bitwidth 4)
    ;;       (set! UP -3)
    ;;       (set! DOWN -4)
    ;;       (set! LEFT -5)
    ;;       (set! RIGHT -6)
    ;;       (set! IO -7))


    (define/public (output-constraint lst [extra-data 0] [extra-return 0])
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
      (progstate a b (>= return 1) (>= data 2) (>= data 1)
                 (if (> (- data 2) 0) (- data 2) 0)
                 (if (> (- return 1) 0) (- return 1) 0)
                 memory #t #t))

    (define (display-state state)
      (pretty-display (format "a:~a b:~a"
			      (progstate-a state) (progstate-b state)))
      (display-data state)
      (display-return state)
      (pretty-display (progstate-memory state))
      (pretty-display (progstate-recv state))
      (pretty-display (progstate-comm state))
      (newline)
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
    
    (define (get-stack stack i)
      (define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
      (vector-ref (stack-body stack) (modulo- (- (stack-sp stack) i) 8)))
    
    (define (set-stack! stack i v)
      (define-syntax-rule (modulo- x y) (if (< x 0) (+ x y) x))
      (vector-set! (stack-body stack) (modulo- (- (stack-sp stack) i) 8) v))

    ;; (define (stack->vector x)
    ;;   (if (stack? x)
    ;;       (for/vector ([i 8]) (get-stack x i))
    ;;       x))

    (define (stack->vector x)
      (if (stack? x)
          (let* ([lst (vector->list (stack-body x))]
                 [p (add1 (stack-sp x))]
                 [a (reverse (take lst p))]
                 [b (reverse (drop lst p))])
            (list->vector (append a b)))
          x))
    
    (define (vector->stack x)
      (if (vector? x)
          (let ([len (vector-length x)])
            (stack 7 (list->vector
                      (reverse (vector->list
                                (vector-append x (make-vector (- 8 len) #f)))))))
          x))
    
    (define (progstate->vector x)
      ;; (when x
      ;;       (pretty-display `(test ,(progstate-data x) ,(progstate-return x) ,(progstate-memory x))))
      (define ret
      (and x
           (vector (progstate-a x)
                   (progstate-b x)
                   (progstate-r x)
                   (progstate-s x)
                   (progstate-t x)
                   (stack->vector (progstate-data x))
                   (stack->vector (progstate-return x))
                   (progstate-memory x)
                   (progstate-recv x)
                   (progstate-comm x))))
      ;; (pretty-display `(ret ,ret))
      ret
      )

    (define (vector->progstate x)
      (and x
           (progstate (vector-ref x 0)
                      (vector-ref x 1)
                      (vector-ref x 2)
                      (vector-ref x 3)
                      (vector-ref x 4)
                      (vector->stack (vector-ref x 5))
                      (vector->stack (vector-ref x 6))
                      (vector-ref x 7)
                      (vector-ref x 8)
                      (vector-ref x 9))))

    (define (parse-state-text str)
      (define tokens (list->vector (string-split str ",")))
      (define-syntax-rule (to-number index) (string->number (vector-ref tokens index)))
      (define a (to-number 1))
      (define b (to-number 2))
      (define r (to-number 3))
      (define s (to-number 4))
      (define t (to-number 5))
      (define raw-data (string-split (vector-ref tokens 6) ";"))
      (define data (stack (string->number (first raw-data))
                          (list->vector 
                           (map string->number (string-split (second raw-data))))))
      (define raw-return (string-split (vector-ref tokens 7) ";"))
      (define return (stack (string->number (first raw-return))
                            (list->vector 
                             (map string->number (string-split (second raw-return))))))
      (define mem-content
        (for/list ([val (map string->number (string-split (vector-ref tokens 8)))]
                   [addr (in-naturals)])
                  (cons addr val)))
      (define memory (new memory-racket% [init (make-hash mem-content)]))
      
      (define recv-content (map string->number (string-split (vector-ref tokens 9))))
      (define recv
        (new queue-in-racket%
             [queue
              (vector-append
               (list->vector recv-content)
               (make-vector (- 4 (length recv-content)) #f))]))
      (define comm (new queue-out-racket%))
      (cons (equal? (vector-ref tokens 0) "#t")
            (progstate a b r s t data return memory recv comm)))

    
    (define/public (constrain-stack precond)
      (define state (get-state (lambda (#:min [min #f] #:max [max #f] #:const [const #f]) const)))
      (when (list? precond)
            (for ([assume precond]
                  [i (reverse (range (length precond)))])
                 (cond
                  [(and assume (= i 0)) (set-progstate-t! state assume)]
                  [(and assume (= i 1)) (set-progstate-s! state assume)]
                  [(and assume (< i 10))
                   (set-stack! (progstate-data state) (- i 2) assume)]

                  [(and assume (<= i 10))
                   (raise "A small function cannot have more than 10 parameters.")]
                  )))
      state)
    
    ;;;;;;;;;;;;;;;;;;;;; program state ;;;;;;;;;;;;;;;;;;;;;;;;

    (define-syntax define-progstate-type-one
      (syntax-rules (min max const)
        ((define-progstate-type-one name get set!)
         (define-progstate-type name
           #:get (lambda (state) (get state))
           #:set (lambda (state val) (set! state val))))
        
        ((define-progstate-type-one name get set! [min min-v] [max max-v])
         (define-progstate-type name
           #:get (lambda (state) (get state))
           #:set (lambda (state val) (set! state val))
           #:min min-v #:max max-v))
        
        ((define-progstate-type-one name get set! [const const-v])
         (define-progstate-type name
           #:get (lambda (state) (get state))
           #:set (lambda (state val) (set! state val))
           #:const const-v))))

    (define (progstate-structure)
      (progstate 'a 'b 'r 's 't
                 (stack 'p-data   (for/vector ([i 8]) 'data))
                 (stack 'p-return (for/vector ([i 8]) 'return))
                 (get-memory-type)
                 (get-queue-in-type)
                 (get-queue-out-type)))
    
    (define-progstate-type-one 'a progstate-a set-progstate-a!)
    (define-progstate-type-one 'b progstate-b set-progstate-b!)
    (define-progstate-type-one 'r progstate-r set-progstate-r!)
    (define-progstate-type-one 's progstate-s set-progstate-s!)
    (define-progstate-type-one 't progstate-t set-progstate-t!)
    (define-progstate-type-one (get-memory-type) progstate-memory set-progstate-memory!)
    (define-progstate-type-one (get-queue-in-type) progstate-recv set-progstate-recv!)
    (define-progstate-type-one (get-queue-out-type) progstate-comm set-progstate-comm!)

    (define-progstate-type 'p-data
      #:get (lambda (state) (stack-sp (progstate-data state)))
      #:set (lambda (state val) (set-stack-sp! (progstate-data state) val))
      #:const 7)
    (define-progstate-type 'data
      #:get (lambda (state arg) (get-stack (progstate-data state)) arg)
      #:set (lambda (state arg val) (raise "Not implemented")))

    (define-progstate-type 'p-return
      #:get (lambda (state) (stack-sp (progstate-return state)))
      #:set (lambda (state val) (set-stack-sp! (progstate-return state) val))
      #:const 7)
    (define-progstate-type 'return
      #:get (lambda (state arg) (get-stack (progstate-return state)) arg)
      #:set (lambda (state arg val) (raise "Not implemented")))
      
    ;;;;;;;;;;;;;;;;;;;;; instruction classes ;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; (set! opcodes '#(nop @p @+ @b @ !+ !b ! +* 2* 
    ;;     		 2/ - + and or drop dup pop over a 
    ;;     		 push b! a!))
    
    (define-arg-type 'const (lambda (config) '(0 1 -1)))

    ;; Use this information to update liveness for everything except data & return stacks.
    ;; Therefore, exclue anything related to those stacks including r, s, and t.
    (init-machine-description 1)
    (define-instruction-class 'nop '(nop))
    (define-instruction-class '@p '(@p)
      #:args '(const) #:ins '(0) #:outs '()) ;; don't bother dealing with data & return stack here.
    (define-instruction-class 'read-a '(@ @+)
      #:args '() #:ins (list 'a (get-memory-type)) #:outs '(a))
    (define-instruction-class 'read-a '(! !+)
      #:args '() #:ins '(a) #:outs (list 'a (get-memory-type)))
    (define-instruction-class 'read-a '(+*)
      #:args '() #:ins '(a) #:outs '(a))
    (define-instruction-class 'read-b '(@b)
      #:args '() #:ins (list 'b (get-memory-type)) #:outs '())
    (define-instruction-class 'read-b '(!b)
      #:args '() #:ins '(b) #:outs (list (get-memory-type)))
    (define-instruction-class 't-only '(2* 2/ - + and or drop dup over push pop)
      #:args '() #:ins '() #:outs '())
    (define-instruction-class 'a '(a)
      #:args '() #:ins '(a) #:outs '())
    (define-instruction-class 'a! '(a!)
      #:args '() #:ins '() #:outs '(a))
    (define-instruction-class 'b! '(b!)
      #:args '() #:ins '() #:outs '(b))
	    
    (finalize-machine-description)

    ;; Inform about the order of argument for load instruction
    (define/override (update-progstate-ins-load my-inst addr state)
      (raise "update-progstate-ins-load: should be called"))

    ;; Inform about the order of argument for store instruction
    (define/override (update-progstate-ins-store my-inst addr val state)
      (raise "update-progstate-ins-load: should be called"))

    (define (dec-live-data live)
      (cond
       [(> (progstate-data live) 0)
        (set-progstate-data! live (sub1 (progstate-data live)))
        live]
       [(progstate-s live)
        (set-progstate-s! live #f)
        live]
       [(progstate-t live)
        (set-progstate-t! live #f)
        live]
       [else live] ;; don't worry about it
       ))

    (define (dec-live-return live)
      (cond
       [(> (progstate-return live) 0)
        (set-progstate-return! live (sub1 (progstate-return live)))
        live]
       [(progstate-r live)
        (set-progstate-r! live #f)
        live]
       [else live] ;; don't worry about it
       ))

    (define (inc-live-data live)
      (cond
       [(> (progstate-data live) 0)
        (set-progstate-data! live (add1 (progstate-data live)))
        live]
       [(progstate-s live)
        (set-progstate-data! live 1)
        live]
       [(progstate-t live)
        (set-progstate-s! live #t)
        live]
       [else 
        (set-progstate-t! live #t)
        live]))

    (define (inc-live-return live)
      (cond
       [(> (progstate-return live) 0)
        (set-progstate-return! live (add1 (progstate-return live)))
        live]
       [(progstate-r live)
        (set-progstate-return! live 1)
        live]
       [else 
        (set-progstate-r! live #t)
        live]))

    (define/override (update-live live my-inst) #f)
    (define/override (update-live-backward live my-inst) #f)

    ))
