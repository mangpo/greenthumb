#lang s-exp rosette

(require  "inst.rkt" "machine.rkt" "memory-rosette.rkt" "queue-rosette.rkt" "special.rkt"
          "ops-rosette.rkt")

(require rosette/solver/smt/z3)
(require rosette/solver/kodkod/kodkod)

(provide validator% sym-input get-rand-func)

;; min & max are inclusive.
(define (sym-input #:min [min #f] #:max [max #f] #:const [const #f])
  (cond
   [const const]
   [else
    (define-symbolic* input number?)
    (when min (assert (>= input min)))
    (when max (assert (<= input max)))
    input]))

(define (get-rand-func bit)
  (lambda (#:min [min-v #f] #:max [max-v #f] #:const [const #f])
    (cond
     [const const]
     [(and min-v max-v) (random-from-list (range min-v (add1 max-v)))]
     [else
      (let* ([rand (random (min 4294967087 (<< 1 bit bit)))]
             [half (arithmetic-shift                           
                    (min 4294967087 (<< 1 bit bit))   
                    -1)])
        ;; (if (>= rand (<< 1 (sub1 bit)))
        ;;     (- rand (<< 1 bit))
        (if (>= rand half)
            (- half rand)
            rand))]
     )))

(define validator%
  (class object%
    (super-new)
    (init-field machine simulator [printer #f]
                [bit (get-field bitwidth machine)]
                [random-input-bit (get-field random-input-bits machine)])
    (public generate-input-states
            counterexample
            get-live-in
            get-sym-vars evaluate-state
            assume assert-state-eq
            get-constructor adjust-memory-config
            )
    
    (define (get-constructor) validator%)
    (define-syntax-rule (display-state x) (send machine display-state x))

    (define ninsts (vector-length (get-field opcodes machine)))
    (define start-time #f)

    (current-solver (new kodkod%))

    ;; Default: no assumption
    (define (assume state assumption)
      (when assumption
            (raise "No support for assumption")))

    (define (interpret-spec spec start-state assumption)
      (assume start-state assumption)
      ;;(pretty-display "interpret spec")
      (define res (send simulator interpret spec start-state))
      ;;(pretty-display "done interpret spec")
      res
      )

    (define (interpret spec start-state)
      (send simulator interpret spec start-state))
    
    ;; Adjust machine config. Specifially, increase memory size if necessary.
    ;; encoded concrete code
    ;; config: machine config
    (define (adjust-memory-config encoded-code)
      (pretty-display (format "solver = ~a" (current-solver)))
      (init-memory-size)
      (define (solve-until-valid)
        (clear-asserts)
	(current-bitwidth bit)
        (define state (send machine get-state sym-input #:concrete #f))
        ;;(pretty-display `(state ,state))

        (with-handlers* 
         ([exn:fail? 
           (lambda (e)
             (if  (equal? (exn-message e) "solve: no satisfying execution found")
                  (begin
                    (increase-memory-size)
                    (solve-until-valid))
                  (raise e)))])
         (solve (send simulator interpret encoded-code state))))

      (define t1 (current-seconds))
      (solve-until-valid)
      (finalize-memory-size)
      (pretty-display "Finish adjusting memory config.")
      (define t2 (current-seconds))
      (pretty-display `(t ,(- t2 t1)))
      )


    (define const-range 
      (for/vector ([i (sub1 random-input-bit)]) (arithmetic-shift 1 i)))
    (define const-range-len (vector-length const-range))
    
        
    (define (rand-func #:min [min-v #f] #:max [max-v #f] #:const [const #f])
      (cond
       [const const]
       [(and min-v max-v) (random-from-list (range min-v (add1 max-v)))]
       [else
        (let* ([rand (random (min 4294967087 (<< 1 random-input-bit bit)))]
               [half (arithmetic-shift                           
                      (min 4294967087 (<< 1 random-input-bit bit))   
                      -1)])
          ;; (if (>= rand (<< 1 (sub1 bit)))
          ;;     (- rand (<< 1 bit))
          (if (>= rand half)
              (- half rand)
              rand))]
       ))

    (define (rand-from-const #:min [min-v #f] #:max [max-v #f] #:const [const #f])
      (cond
       [const const]
       [(and min-v max-v) (random-from-list (range min-v (add1 max-v)))]
       [else (vector-ref const-range (random const-range-len))]
       ))


    (define (generate-input-states-fast n spec assumption #:db [db #f])
      (define m (if db n (quotient (add1 n) 2)))
      (define inputs-random
        (for/list ([i m]) (send machine get-state rand-func)))
      (define inputs-random-const
        (for/list ([i (- n m)]) (send machine get-state rand-from-const)))
      (define inputs (append inputs-random inputs-random-const))
      (define pass
        (with-handlers* 
         ([exn:fail? (lambda (e) #f)])
         (for ([input inputs])
              (assume input assumption)
              (send simulator interpret spec input))
         #t))
      (and pass inputs)
      )
        
    (define/public (generate-input-states-slow n spec assumption #:db [db #f] #:raw [raw #f])
      (when debug
            (pretty-display `(generate-inputs-inner ,n ,assumption ,random-input-bit)))
      (clear-asserts)
      (current-bitwidth bit)
      (define start-state (send machine get-state sym-input #:concrete #f))

      (define sols (list))
      (define first-solve #t)
      (define (loop [extra #t] [count n])
        ;;(pretty-display `(loop ,extra ,n))
        (define (assert-extra-and-interpret)
          ;; Assert that the solution has to be different.
          (assert extra)
          (assume start-state assumption)
          (interpret spec start-state)
          )
        (define sol (solve (assert-extra-and-interpret)))
        ;;(pretty-display `(state ,start-state))
        (define restrict-pairs (solution->list sol))
        (set! first-solve #f)
        (unless (empty? restrict-pairs)
                (set! sols (cons sol sols))
                (when (> count 1)
                      (loop 
                       (and extra (ormap (lambda (x) (not (equal? (car x) (cdr x)))) restrict-pairs))
                       (sub1 count)))))
      
      (with-handlers* 
       ([exn:fail? 
         (lambda (e)
           (if  (equal? (exn-message e) "solve: no satisfying execution found")
                (if first-solve
                    (raise "Cannot construct valid inputs.")
                    (when debug (pretty-display "no more!")))
                (raise e)))])
       (loop))
      
      (define const-range 
	;; (- (arithmetic-shift 1 (sub1 random-input-bit)))
	(for/vector ([i (sub1 random-input-bit)]) (arithmetic-shift 1 i)))
      (define const-range-len (vector-length const-range))
      
      (define (generate-one-input random-f)
        (make-hash 
         (for/list ([v sym-vars]) 
                   (let ([val (random-f)])
                     (cons v val)))))
      
      (define sym-vars (get-sym-vars start-state))
      
      (define m (if db n (quotient (add1 n) 2)))
      ;; Random
      (define input-random (for/list ([i m]) (generate-one-input rand-func)))
      ;; Random in const list
      (define input-random-const (for/list ([i (- n m)]) (generate-one-input rand-from-const)))
      
      (define inputs (append input-random input-random-const))

      ;; (when debug
      ;;       (pretty-display "Test simulate with symbolic inputs...")
      ;;       (assume start-state assumption)
      ;;       (interpret spec start-state)
      ;;       (pretty-display "Passed!"))
      ;; Construct cnstr-inputs.

      (define cnstr-inputs (list))
      (for ([sol sols])
           (define restrict-pairs (list))
           (set! first-solve #f)
           (for ([pair (solution->list sol)])
                ;; Filter only the ones that matter.
                (when (hash-has-key? (car inputs) (car pair))
                      (set! restrict-pairs (cons pair restrict-pairs))))
        (unless (empty? restrict-pairs)
                (set! cnstr-inputs (cons restrict-pairs cnstr-inputs))))
      
      (set! cnstr-inputs (list->vector cnstr-inputs))
      (define cnstr-inputs-len (vector-length cnstr-inputs))
      (when debug (pretty-display `(cnstr-inputs ,cnstr-inputs-len ,cnstr-inputs)))
      
      ;; Modify inputs with cnstr-inputs
      (when (> cnstr-inputs-len 0)
            (for ([i n]
                  [input inputs])
                 (let ([cnstr-input (vector-ref cnstr-inputs (modulo i cnstr-inputs-len))])
                   (for ([pair cnstr-input])
                        (hash-set! input (car pair) (cdr pair))))))

      (if raw
          (for/list ([input inputs])
                    (let ([sol (sat (make-immutable-hash (hash->list input)))])
                      sol))
          (for/list ([input inputs])
                    (let ([sol (sat (make-immutable-hash (hash->list input)))])
                      (evaluate-state start-state sol)))))
    
    ;; Generate input states.
    (define (generate-input-states n spec assumption #:db [db #f])
      (pretty-display "Generate inputs (fast).")
      (define states (generate-input-states-fast n spec assumption #:db db))

      (cond
       [states states]
       [else
        (pretty-display "Generate inputs (slow).")
        (generate-input-states-slow n spec assumption #:db db)]))
      

    ;; Returns a counterexample if spec and program are different.
    ;; Otherwise, returns false.
    (define (counterexample spec program constraint
                            #:assume [assumption (send machine no-assumption)])
      ;;(pretty-display (format "solver = ~a" (current-solver)))
      (when (and debug printer)
	    (pretty-display `(counterexample ,bit))
	    (pretty-display `(spec))
	    (send printer print-syntax (send printer decode spec))
	    (pretty-display `(program))
	    (send printer print-syntax (send printer decode program))
	    (pretty-display `(constraint ,constraint))
	    (pretty-display `(assumption ,assumption))
	    )
      
      (clear-asserts)
      (current-bitwidth bit)
      (define start-state (send machine get-state sym-input #:concrete #f))
      (define spec-state #f)
      (define program-state #f)
      
      (define (interpret-spec!)
        (set! spec-state
              (if (procedure? spec)
                  (spec start-state) ;; TODO: handle assumption
                  (interpret-spec spec start-state assumption)))
        )
      
      (define (compare)
        (set! program-state (send simulator interpret program start-state spec-state))
        (assert-state-eq spec-state program-state constraint)
        )

      ;; VERIFY
      (with-handlers* 
       ([exn:fail? 
         (lambda (e)
           (when debug (pretty-display "program-eq? SAME"))
           (unsafe-clear-terms!)
           (if (equal? (exn-message e) "verify: no counterexample found")
               #f
               (raise e)))])
       (let ([model (verify #:assume (interpret-spec!) #:guarantee (compare))])
         (when debug (pretty-display "program-eq? DIFF"))
         (let ([state (evaluate-state start-state model)])
           (unsafe-clear-terms!)
           state)
         )))
    
    ;; Return live-in in progstate format.
    ;; live-out: progstate format
    ;; extra: extra information
    (define (get-live-in code live-out)
      ;;(pretty-display `(live-out ,live-out))
      (define in-state (send machine get-state sym-input #:concrete #f))
      (define out-state (interpret code in-state))
      (define vec-live-out (send machine progstate->vector live-out))
      (define vec-input (send machine progstate->vector in-state))
      (define vec-output (send machine progstate->vector out-state))
      ;;(pretty-display `(in-state ,in-state))
      
      (define live-list (list))
      (define (collect-sym pred x)
        (cond
         [(is-a?* x memory-rosette%) ;; TODO: treat all memory update to be live.
          (define init (filter pair? (vector->list  (get-field* init x))))
          (define update (filter pair? (vector->list (get-field* update x))))
          (set! live-list (append (map car init) (map cdr init) live-list))
          (set! live-list (append (map car update) (map cdr update) live-list))]
         [(is-a?* x queue-in-rosette%) (void)]
         [(is-a?* x queue-out-rosette%)
          (define queue (vector->list (get-field* queue x)))
          (set! live-list (append queue live-list))]
         [(boolean? pred)
          ;; (pretty-display `(collect-sym ,pred ,x))
          (when pred (set! live-list (cons x live-list))
                ;; (pretty-display `(add ,(symbolics x)))
                )]
         [(number? pred)
          (for ([p pred] [i x]) 
               (collect-sym #t i))]
         [(pair? x)
          (collect-sym (car pred) (car x))
          (collect-sym (cdr pred) (cdr x))]
         [else
          (for ([p pred] [i x]) (collect-sym p i))]))

      ;; (pretty-display `(vec-input ,vec-input))
      ;; (pretty-display `(vec-output ,vec-output))
      ;; (pretty-display `(vec-live-out ,vec-live-out))
      (collect-sym vec-live-out vec-output)
      (define live-terms (list->set (symbolics live-list)))
      ;; (pretty-display `(vec-input ,vec-input))
      ;;(pretty-display `(live-terms ,(set->list live-terms)))
      
      (define (extract-live pred x)
	;;(pretty-display `(extract-live ,pred ,x ,(is-a?* x special%) ,(is-a? x memory-rosette%)))
        (cond
         [(number? pred)
          (define index 0)
          (for ([ele x]
                [i (vector-length x)])
               (when (set-member? live-terms ele) (set! index (add1 i))))
          index]
         [(number? x) 
          (if (term? x)
              (set-member? live-terms x)
              pred)]
	 [(and (vector? x) (vector? pred)) 
	  (for/vector ([i x] [p pred]) (extract-live p i))]
         [(vector? x) 
	  (for/or ([i x]) (extract-live pred i))]
         ;; [(vector? x) 
	 ;;  (for/vector ([i x]) (extract-live pred i))]
         ;; [(boolean? pred) ;;(pretty-display `(return ,pred)) 
	 ;;  pred]
         [(pair? x) 
          (cons (extract-live (car pred) (car x)) 
                (extract-live (cdr pred) (cdr x)))]
         [(list? x)
          (for/list ([i x] [p pred]) (extract-live p i))]
         ;;[(is-a?* x special%) #t]
         [(is-a?* x memory-rosette%)
          (or pred (not (empty? (symbolics (get-field* init x)))))]
         [(is-a?* x queue-in-rosette%) #t]
         [(is-a?* x queue-out-rosette%) pred]
         [else pred]
         ))

      (send machine vector->progstate (extract-live vec-live-out vec-input)))
      
    ;; Assert that state1 and state2 are equal where pred is #t.
    ;; state1, state2, & pred: progstate format
    (define (assert-state-eq state1 state2 pred)
      (define (inner state1 state2 pred)
        ;;(pretty-display `(assert-eq ,pred ,state1 ,state2))
	(cond
         [(and pred (is-a?* state1 memory-rosette%))
          (assert (equal? (get-field* update state1)
                          (get-field* update state2)))]
         
         [(and pred (or (is-a?* state1 queue-in-rosette%)
                        (is-a?* state2 queue-out-rosette%)))
          (assert (equal? (get-field* queue state1)
                          (get-field* queue state2)))]
         
	 [(equal? pred #t)
          (for*/all ([i state2])
                    (assert (equal? state1 i)))
          ]
	 [(equal? pred #f)
	  (void)]
	 [else
	  (for/and ([i pred]
		    [s1 state1]
		    [s2 state2])
		   (inner s1 s2 i))])
	)
      (inner state1 state2 pred)
      )

    ;; Evaluate symbolic progstate to concrete progstate based on solution 'sol'.
    (define (evaluate-state state sol)
      (define sol-list (solution->list sol))
      (define sol-hash (make-hash sol-list))
      (define sym-vars (get-sym-vars state))

      (for ([var sym-vars])
           (unless (hash-has-key? sol-hash var)
                   (set! sol-list (cons (cons var 0) sol-list))))
      (set! sol (sat (make-immutable-hash sol-list)))
      ;;(pretty-display `(sol ,sol))
      
      (define-syntax-rule (eval x model)
        (let ([ans (evaluate x model)])
          ;;(pretty-display `(eval ,x ,ans))
          ans))

      (define-syntax-rule (concretize x)
        (if (term? x) 0 x))
      
      (define (inner x)
        ;;(pretty-display `(inner ,x))
        (cond
         [(vector? x) (for/vector ([i x]) (inner i))]
         [(list? x) (for/vector ([i x]) (inner i))]
         [(pair? x) (cons (inner (car x)) (inner (cdr x)))]
         [(is-a? x special%)
          (send x create-concrete (lambda (x) (inner (eval x sol))))]
         [else (concretize x)]))
      (define ret
        (inner (eval state sol)))
      ;;(pretty-display `(ret ,ret))
      ret
      )

    ;; Get all symbolic variables in state.
    ;; state: progstate format
    (define (get-sym-vars state)
      (define lst (list))
      (define (add x)
        (when (term? x)
              (set! lst (cons x lst))))

      (define (inner x)
	(cond
	 [(or (list? x) (vector? x))
	  (for ([i x]) (inner i))]
	 [(pair? x)
	  (inner (car x)) (inner (cdr x))]
         [(is-a? x memory-rosette%)    (inner (get-field init x))]
         [(is-a? x queue-in-rosette%)  (inner (get-field init x))]
         [(is-a? x queue-out-rosette%) (inner (get-field queue x))]
	 [else (add x)]))
      (define converted (send machine progstate->vector state))
      (inner converted)
      (set->list (list->set (symbolics lst)))
      )

    
    ))
