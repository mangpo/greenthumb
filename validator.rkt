#lang s-exp rosette

(require  "ast.rkt" "machine.rkt" "printer.rkt")

(require rosette/solver/smt/z3)
;(require rosette/solver/kodkod/kodkod)

(provide validator%)

(define validator%
  (class object%
    (super-new)
    (init-field machine printer 
                [simulator #f] 
                [bit (get-field bit machine)]
                [random-input-bit (get-field random-input-bit machine)]
                [time #f])
    ;; (abstract get-sym-vars evaluate-state
    ;;           assume assert-state-eq)
    (public proper-machine-config generate-input-states generate-inputs-inner
            counterexample
            sym-op sym-arg sym-insts ;; do really need sym-insts
            evaluate-inst encode-sym-inst encode-sym
            assume-relax get-live-in
            get-sym-vars evaluate-state
            assume assert-state-eq
            )
    
    (current-solver (new z3%))

    (define-syntax-rule (print-struct x) (send printer print-struct x))
    (define-syntax-rule (print-syntax x) (send printer print-syntax x))
    (define-syntax-rule (decode x) (send printer decode x))
    (define-syntax-rule (display-state x) (send machine display-state x))

    (define ninsts (vector-length (get-field inst-id machine)))
    (define start-time #f)

    (define/public (reset) (current-solver (new z3%)))

    (define (sym-input)
      (define-symbolic* input number?)
      input
      )


    (define (sym-op)
      (define-symbolic* op number?)
      (assert (and (>= op 0) (< op ninsts)))
      op)
    
    (define (sym-arg)
      (define-symbolic* arg number?)
      arg)

    (define (encode-sym-inst x)
      (if (inst-op x)
          (send printer encode-inst x)
          (inst (sym-op) (sym-arg))))

    (define (encode-sym code)
      (traverse code inst? encode-sym-inst))

    (define (sym-insts size)
      (encode-sym (for/vector ([i size]) (inst #f #f))))

    (define (assume-relax state assumption)
      (assume state assumption))

    ;; Default: no assumption
    (define (assume state assumption)
      (when assumption
            (raise "No support for assumption")))

    (define (evaluate-inst x model)
      (inst (evaluate (inst-op x) model)
            (evaluate (inst-args x) model)))

    (define (evaluate-program code model)
      (traverse code inst? (lambda (x) (evaluate-inst x model))))

    (define (interpret-spec spec start-state assumption)
      (assume start-state assumption)
      ;;(pretty-display "interpret spec")
      (define res (send simulator interpret spec start-state))
      ;;(pretty-display "done interpret spec")
      res
      )

    (define (interpret spec start-state)
      (send simulator interpret spec start-state))
    
    ;; code: non-encoded concrete code
    ;; config: machine config
    (define (proper-machine-config code config [extra #f])
      (pretty-display `(config ,config))
      (define encoded-code (encode-sym code))
      (define (solve-until-valid config)
        (send machine set-config config)
        (clear-asserts)
	(current-bitwidth bit)
        (define state (send machine get-state sym-input extra))
	(send simulator interpret encoded-code state)

        (with-handlers* 
         ([exn:fail? 
           (lambda (e)
             (if  (equal? (exn-message e) "solve: no satisfying execution found")
                  (let ([new-config (send machine adjust-config config)])
                    (if (send machine config-exceed-limit? new-config)
                        (raise "Cannot find inputs to the program for the memory size < 1000.
1) Try increasing memory size when calling (set-machine-config).
2) Some operation in interpret.rkt might not be legal for Rosette's symbolic object.")
                        (solve-until-valid new-config)))
                  (raise e)))])
         (solve (send simulator interpret encoded-code state))
         (send machine finalize-config config)))
      
      (solve-until-valid config))
    
    (define (generate-inputs-inner 
             n spec start-state assumption
             #:rand-func
             [rand-func (lambda () (random (min 4294967087 (<< 1 random-input-bit))))]
             #:db [db #f])
      (when debug
            (pretty-display `(generate-inputs-inner ,n ,assumption ,random-input-bit)))
      (clear-asserts)
      (current-bitwidth bit)
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

      ;; All 0s
      (define input-zero (list (generate-one-input (lambda () 0))))
      
      (define m (if db n (quotient n 2)))
      ;; Random
      (define input-random
        (for/list ([i m])
                  (generate-one-input 
                   (lambda () (let ([rand (rand-func)])
                                (if (>= rand (<< 1 (sub1 bit)))
                                    (- rand (<< 1 bit))
                                    rand))))))
      
      ;; Random in const list
      (define input-random-const
        ;; (for/list ([i (- n m 1)])
        (for/list ([i (- n m)])
                  (generate-one-input 
                   (lambda () 
                     (vector-ref const-range (random const-range-len))))))
      
      ;;(define inputs (append input-zero input-random input-random-const))
      (define inputs (append input-random input-random-const))
      ;; (when debug
      ;;       (pretty-display "Test simulate with symbolic inputs...")
      ;;       (assume-relax start-state assumption)
      ;;       (interpret spec start-state)
      ;;       (pretty-display "Passed!"))
      ;; Construct cnstr-inputs.
      (define cnstr-inputs (list))
      (define first-solve #t)
      (define (loop [extra #t] [count n])
        (define (assert-extra-and-interpret)
          ;; Assert that the solution has to be different.
          (assert extra)
          (assume-relax start-state assumption)
          (interpret spec start-state)
          )
        (define sol (solve (assert-extra-and-interpret)))
        (define restrict-pairs (list))
        (set! first-solve #f)
        (for ([pair (solution->list sol)])
             ;; Filter only the ones that matter.
             (when (hash-has-key? (car inputs) (car pair))
                   (set! restrict-pairs (cons pair restrict-pairs))))
        (unless (empty? restrict-pairs)
                (set! cnstr-inputs (cons restrict-pairs cnstr-inputs))
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
      
      (set! cnstr-inputs (list->vector (reverse cnstr-inputs)))
      (define cnstr-inputs-len (vector-length cnstr-inputs))
      (when debug (pretty-display `(cnstr-inputs ,cnstr-inputs-len ,cnstr-inputs)))
      
      ;; Modify inputs with cnstr-inputs
      (when (> cnstr-inputs-len 0)
            (for ([i n]
                  [input inputs])
                 (let ([cnstr-input (vector-ref cnstr-inputs (modulo i cnstr-inputs-len))])
                   (for ([pair cnstr-input])
                        (hash-set! input (car pair) (cdr pair))))))
      
      (values sym-vars 
              (map (lambda (x) (sat (make-immutable-hash (hash->list x)))) inputs)))

    (define (generate-input-states 
             n spec assumption [extra #f]
             #:rand-func 
             [rand-func (lambda () (random (min 4294967087 (<< 1 random-input-bit))))]
             #:db [db #f])
      (define start-state (send machine get-state sym-input extra))
      (define-values (sym-vars sltns)
        (generate-inputs-inner n spec start-state assumption 
                               #:rand-func rand-func
                               #:db db))
      (map (lambda (x) (evaluate-state start-state x)) sltns))

    ;; Returns a counterexample if spec and program are different.
    ;; Otherwise, returns false.
    (define (counterexample spec program constraint [extra #f]
                            #:assume [assumption (send machine no-assumption)])
      ;;(pretty-display (format "solver = ~a" (current-solver)))
      (when debug 
            (pretty-display (format "program-eq? START bit = ~a" bit))
            (pretty-display "spec:")
            (print-struct spec)
            (pretty-display "program:")
            (print-struct program)
            ;; (pretty-display "constraint:")
            ;; (display-state constraint)
            ;; (pretty-display (format "assumption: ~a" assumption))
            )
      
      (clear-asserts)
      (current-bitwidth bit)
      (define start-state (send machine get-state sym-input extra))
      (define spec-state #f)
      (define program-state #f)
      
      (define (interpret-spec!)
        ;;(pretty-display ">>> interpret spec")
        (set! spec-state (interpret-spec spec start-state assumption))
        ;;(pretty-display ">>> done interpret spec")
        )
      
      (define (compare)
        ;;(pretty-display ">>> interpret program")
        (set! program-state (send simulator interpret program start-state spec-state))
        ;;(pretty-display ">>> done interpret program")
        
        ;; (pretty-display ">>>>>>>>>>> SPEC >>>>>>>>>>>>>")
        ;; (display-state spec-state)
        ;; (pretty-display ">>>>>>>>>>> PROG >>>>>>>>>>>>>")
        ;; (display-state program-state)
        
        ;;(pretty-display "check output")
        ;; (pretty-display constraint)
        (assert-state-eq spec-state program-state constraint)
        ;;(pretty-display "done check output")
        )

      (send time start `z3)
      (with-handlers* 
       ([exn:fail? 
         (lambda (e)
           (send time end `z3)
           (when debug (pretty-display "program-eq? SAME"))
           (clear-terms!)
           (if (equal? (exn-message e) "verify: no counterexample found")
               #f
               (raise e)))])
       (let ([model (verify #:assume (interpret-spec!) #:guarantee (compare))])
         (send time end `z3)
         (when debug (pretty-display "program-eq? DIFF"))
         (let ([state (evaluate-state start-state model)])
           ;; (pretty-display model)
           ;; (display-state state)
           ;; (raise "done")
           (clear-terms!)
           state)
         )))
    
    ;; Return live-in in progstate format.
    (define (get-live-in code live-out extra)
      (define in-state (send machine get-state-liveness sym-input extra))
      (define out-state (interpret code in-state))
      (define vec-live-out (send machine progstate->vector live-out))
      (define vec-input (send machine progstate->vector in-state))
      (define vec-output (send machine progstate->vector out-state))
      
      (define live-list (list))
      (define (collect-sym pred x)
        (cond
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

      ;; (pretty-display `(vec-live-out ,vec-live-out))
      ;; (pretty-display `(vec-output ,vec-output))
      (collect-sym vec-live-out vec-output)
      (define live-terms (list->set (symbolics live-list)))
      ;; (pretty-display `(live-terms ,live-terms))
      
      (define (extract-live pred x)
        (cond
         [(boolean? pred) 
          (if (term? x)
              (set-member? live-terms x)
              pred)]
         [(number? pred)
          (define index 0)
          (for ([ele x]
                [i (vector-length x)])
               (when (set-member? live-terms ele) (set! index (add1 i))))
          index]
         [(pair? x) 
          (cons (extract-live (car pred) (car x)) 
                (extract-live (cdr pred) (cdr x)))]
         [(list? x) (for/list ([i x] [p pred]) (extract-live p i))]
         [(vector? x) (for/vector ([i x] [p pred]) (extract-live p i))]))

      (send machine vector->progstate (extract-live vec-live-out vec-input)))
      
  
    (define (assert-state-eq state1 state2 pred)
      (define (inner state1 state2 pred)
	(cond
	 [(equal? pred #t)
	  (assert (equal? state1 state2))]
	 [(equal? pred #f)
	  (void)]
	 [(number? pred)
	  (for/and ([i pred]
		    [s1 state1]
		    [s2 state2])
		   (assert (equal? s1 s2)))]
	 [else
	  (for/and ([i pred]
		    [s1 state1]
		    [s2 state2])
		   (inner s1 s2 i))])
	)
      (inner (send machine progstate->vector state1)
	     (send machine progstate->vector state2)
	     (send machine progstate->vector pred))
      )

    (define (evaluate-state state sol)
      (define-syntax-rule (eval x model)
        (let ([ans (evaluate x model)])
          (if (term? ans) 0 ans)))

      (define (inner x)
	(cond
	 [(vector? x) (for/vector ([i x]) (inner i))]
	 [(list? x) (for/vector ([i x]) (inner i))]
	 [(pair? x) (cons (inner (car x)) (inner (cdr x)))]
	 [else (eval x sol)]))
      (send machine vector->progstate
	    (inner (send machine progstate->vector state))))

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
	 [else (add x)]))
      (inner (send machine progstate->vector state))
      lst)
    
    ))
