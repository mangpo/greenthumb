#lang s-exp rosette

(require  "ast.rkt" "machine.rkt" "printer.rkt")

(require rosette/solver/z3/z3)
(require rosette/solver/kodkod/kodkod)

(provide solver%)

(define solver%
  (class object%
    (super-new)
    (init-field machine printer 
                [simulator #f] 
                [bit (get-field bit machine)]
                [random-input-bit (get-field random-input-bit machine)])
    (abstract get-sym-vars evaluate-state
              encode-sym decode-sym sym-insts
              assume assert-output)
    (public proper-machine-config generate-input-states
            superoptimize superoptimize-binary synthesize-from-sketch counterexample
            sym-op sym-arg
            assume-relax get-live-in)

    (define-syntax-rule (print-struct x) (send printer print-struct x))
    (define-syntax-rule (display-state x) (send machine display-state x))

    (define ninsts (vector-length (get-field inst-id machine)))

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

    (define (assume-relax state assumption)
      (assume state assumption))

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
      (define encoded-code (encode-sym code))
      (define (solve-until-valid config)
        (send machine set-config config)
        ;; (current-solver (new kodkod%))
        (clear-asserts)
        (configure [bitwidth bit] [loop-bound 20])
        (define state (send machine get-state sym-input extra))
	;;(send simulator interpret encoded-code state)

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
         config))
      
      (solve-until-valid config))
    
    (define (generate-inputs-inner n spec start-state assumption)
      (pretty-display `(generate-inputs-inner ,n ,assumption ,random-input-bit))
      ;; (print-struct spec)
      ;; (display-state start-state)
      ;; (current-solver (new kodkod%))
      (clear-asserts)
      (configure [bitwidth bit] [loop-bound 20])
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
      (define input-zero (list (make-hash (for/list ([v sym-vars]) (cons v 0)))))
      
      (define m (quotient n 2))
      ;; Random
      (define input-random
        (for/list ([i m])
                  (generate-one-input 
                   (lambda () (let ([rand (random (min 4294967087 
                                                       (<< 1 random-input-bit)))])
                                (if (>= rand (<< 1 (sub1 bit)))
                                    (- rand (<< 1 bit))
                                    rand))))))
      
      ;; Random in const list
      (define input-random-const
        (for/list ([i (- n m 1)])
                  (generate-one-input 
                   (lambda () 
                     (vector-ref const-range (random const-range-len))))))
      
      (define inputs (append input-zero input-random input-random-const))
      (when debug
            (pretty-display "Test simulate with symbolic inputs...")
            (assume-relax start-state assumption)
            (interpret spec start-state)
            (pretty-display "Passed!"))
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

    (define (generate-input-states n spec assumption [extra #f])
      (define start-state (send machine get-state sym-input extra))
      (define-values (sym-vars sltns)
        (generate-inputs-inner n spec start-state assumption))
      (map (lambda (x) (evaluate-state start-state x)) sltns))


    (define (superoptimize-inc spec constraint name time-limit size [extra #f]
			   #:assume [assumption (send machine no-assumption)])
      (synthesize-from-sketch-inc spec 
			      (sym-insts (if size size (vector-length spec)))
			      constraint extra
			      (send simulator performance-cost spec)
			      time-limit name
			      #:assume assumption))

    ;; Superoptimize program
    ;; >>> INPUTS >>>
    ;; spec: program specification (naive code)
    ;; sketch: skeleton of the output program
    ;; constraint: constraint on the output state
    ;; cost: upperbound (exclusive) of the cost of the output program, #f is no upperbound
    ;; assume-interpret: always true (for now)
    ;; assume: input assumption
    (define (synthesize-from-sketch-inc spec sketch constraint extra cost time-limit name
				    #:assume-interpret [assume-interpret #t]
				    #:assume [assumption (send machine no-assumption)])
      (pretty-display (format "SUPERPOTIMIZE-INC: assume-interpret = ~a" assume-interpret))

      ;; (current-solver (new z3%))
      (current-solver (new kodkod-incremental%))
      (clear-asserts)
      (configure [bitwidth bit] [loop-bound 20])
      (define start-time (current-seconds))
      (define start-state (send machine get-state sym-input extra))
      (define spec-state #f)
      (define sketch-state #f)
      (define spec-cost #f)
      (define sketch-cost #f)
      
      (define (interpret-spec!)
        (pretty-display "========== interpret spec")
        (set! spec-state (interpret-spec spec start-state assumption)))
      
      (define (compare-spec-sketch)
        (pretty-display "=========== interpret sketch")
        (set! sketch-state (send simulator interpret sketch start-state spec-state))
        (pretty-display "check output")
        ;; (set! spec-cost (send simulator performance-cost spec))
        (set! sketch-cost (send simulator performance-cost sketch))
        (when cost (assert (< sketch-cost cost)))
        (assert-output spec-state sketch-state constraint)
        )
      
      ;; Collect input variables and contruct their init values.
      (define-values (sym-vars inputs)
        (generate-inputs-inner 2 spec start-state assumption))

      ;; (when debug
      ;;       (pretty-display "Test calculate performance-cost with symbolic instructions...")
      ;;       (send simulator performance-cost sketch)
      ;;       (pretty-display "Test simulate with symbolic instructions...")
      ;;       (send simulator interpret sketch start-state)
      ;;       (pretty-display "Passed!"))
      
      (define final-program #f)
      (define (inner)
	(define model 
	  (timeout
	   time-limit
	   (synthesize 
	    #:forall sym-vars
	    #:init inputs
	    #:assume (if assume-interpret (interpret-spec!) (assume start-state assumption))
	    #:guarantee (compare-spec-sketch)
	    )
	   )
	  )
	
	(pretty-display ">>> done synthesize")
	(set! final-program (decode-sym sketch model))
	(define final-cost (evaluate sketch-cost model))
	;; Print to file
	(with-output-to-file #:exists 'truncate (format "~a.stat" name)
	  (thunk
	   (pretty-display (format "best-correct-cost:\t~a" final-cost))
	   (pretty-display (format "best-correct-time:\t~a" 
				   (- (current-seconds) start-time)))))
	(with-output-to-file #:exists 'truncate (format "~a.best" name)
	  (thunk
	   (send printer print-syntax final-program)))
	
	(pretty-display ">>> superoptimize-output")
	(print-struct final-program)
	(pretty-display (format "limit cost = ~a" cost))
	(pretty-display (format "new cost = ~a" final-cost))
	(pretty-display "=====================================")
	(when cost 
	      (set! cost final-cost)
	      (inner)))

      (with-handlers* 
       ([exn:fail? 
	 (lambda (e) 
	   (clear-asserts)
	   (clear-terms!)
	   (if (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
		   (regexp-match #rx"assert: progstate-cost" (exn-message e)))
	       final-program
	       (raise e)))]
	[exn:break? (lambda (e) 
		      (clear-asserts)
		      (clear-terms!)
		      (if final-program
			  final-program
			  "timeout"))])
       (inner)
       final-program))

    (define (remove-nops code)
      (list->vector 
       (filter (lambda (x) (not (equal? (inst-op x) "nop")))
               (vector->list code))))

    ;; Optimize the cost using binary search on the number of holes.
    ;; spec: non-encoded block
    (define (superoptimize-binary spec constraint live-in name time-limit size [extra #f]
                                  #:assume [assumption (send machine no-assumption)]
                                  #:input-file [input-file #f]
                                  #:start-prog [start #f])
      (define start-time (current-seconds))
      (define final-program #f)
      (define final-len (if size size (vector-length spec)))
      (define final-cost #f)
      (define (inner begin end cost)
        (define middle (quotient (+ begin end) 2))
        (pretty-display `(binary-search ,begin ,end ,middle))
        (define sketch (sym-insts middle))
        
        (define-values (out-program out-cost)
          (with-handlers* 
           ([exn:fail? 
             (lambda (e) 
               (pretty-display "catch error")
               (if (regexp-match #rx"synthesize: synthesis failed" 
                                 (exn-message e))
                   (values #f cost)
                   (raise e)))])
           (synthesize-from-sketch spec sketch constraint extra cost time-limit
                                   #:assume assumption)))

        (pretty-display `(out ,out-program ,out-cost))

        (when out-program 
              ;; Print to file
              (with-output-to-file #:exists 'truncate (format "~a.stat" name)
                (thunk
                 (pretty-display (format "best-correct-cost:\t~a" out-cost))
                 (pretty-display (format "best-correct-time:\t~a" 
                                         (- (current-seconds) start-time)))))
              (with-output-to-file #:exists 'truncate (format "~a.best" name)
                (thunk
                 (send printer print-syntax out-program)))
              (set! final-program out-program)
              (set! final-len middle)
              (set! final-cost out-cost))

        (if out-program
            (inner begin middle out-cost)
            (when (< middle end) (inner (add1 middle) end cost))))
      
      (with-handlers 
       ([exn:break? (lambda (e) (unless final-program (set! final-program "timeout")))])
       (inner 1 final-len #f))

      ;; Try len + 2
      ;; (unless (equal? final-program "timeout")
      ;;         (with-handlers 
      ;;          ([exn:break? (lambda (e) (void))])
      ;;          (inner (+ final-len 2) (+ final-len 2) final-cost)))
      

      (pretty-display "after inner")
      final-program)

    (define (superoptimize spec constraint live-in name time-limit size [extra #f]
			   #:assume [assumption (send machine no-assumption)]
                           #:input-file [input-file #f]
                           #:start-prog [start #f])
      (define sketch (sym-insts (if size size (vector-length spec))))
      (define start-time (current-seconds))
      (define final-program #f)
      (define (inner cost)
	(define-values (out-program out-cost) 
	  (synthesize-from-sketch spec sketch constraint extra cost time-limit
				  #:assume assumption))
	;; Print to file
	(with-output-to-file #:exists 'truncate (format "~a.stat" name)
	  (thunk
	   (pretty-display (format "best-correct-cost:\t~a" out-cost))
	   (pretty-display (format "best-correct-time:\t~a" 
				   (- (current-seconds) start-time)))))
	(with-output-to-file #:exists 'truncate (format "~a.best" name)
	  (thunk
	   (send printer print-syntax out-program)))

	(set! final-program out-program)
	(set! sketch (vector-take sketch (vector-length (remove-nops final-program))))
	(inner out-cost))
      
      (with-handlers* 
       ([exn:fail? 
	 (lambda (e) 
	   (clear-terms!)
	   (if (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
		   (regexp-match #rx"assert: progstate-cost" (exn-message e)))
	       final-program
	       (raise e)))]
	[exn:break? (lambda (e) 
		      (clear-terms!)
		      (if final-program
			   final-program
			  "timeout"))])
       (inner (send simulator performance-cost spec))))

    ;; Superoptimize program
    ;; >>> INPUTS >>>
    ;; spec: program specification (naive code)
    ;; sketch: skeleton of the output program
    ;; constraint: constraint on the output state
    ;; cost: upperbound (exclusive) of the cost of the output program, #f is no upperbound
    ;; assume-interpret: always true (for now)
    ;; assume: input assumption
    (define (synthesize-from-sketch spec sketch constraint extra 
				    [cost #f]
				    [time-limit 3600]
				    #:assume-interpret [assume-interpret #t]
				    #:assume [assumption (send machine no-assumption)])
      (pretty-display (format "SUPERPOTIMIZE: assume-interpret = ~a" assume-interpret))
      (when debug (send printer print-struct sketch))

      ;; (current-solver (new z3%))
      ;; (current-solver (new kodkod%))

      (clear-asserts)
      (configure [bitwidth bit] [loop-bound 20])

      (define start-state (send machine get-state sym-input extra))
      (define spec-state #f)
      (define sketch-state #f)
      (define spec-cost #f)
      (define sketch-cost #f)
      ;; (pretty-display "========= start state")
      ;; (send machine display-state start-state)
      
      (define (interpret-spec!)
        (pretty-display "========== interpret spec")
        (set! spec-state (interpret-spec spec start-state assumption)))
      
      (define (compare-spec-sketch)
        (pretty-display "=========== interpret sketch")
        (set! sketch-state (send simulator interpret sketch start-state spec-state))
        (pretty-display "check output")
        ;; (set! spec-cost (send simulator performance-cost spec))
        (set! sketch-cost (send simulator performance-cost sketch))
        (when cost (assert (< sketch-cost cost)))
        (assert-output spec-state sketch-state constraint)
        )
      
      ;; Collect input variables and contruct their init values.
      (define-values (sym-vars inputs)
        (generate-inputs-inner 2 spec start-state assumption))

      ;; (when debug
      ;;       (pretty-display "Test calculate performance-cost with symbolic instructions...")
      ;;       (send simulator performance-cost sketch)
      ;;       (pretty-display "Test simulate with symbolic instructions...")
      ;;       (send simulator interpret sketch start-state)
      ;;       (pretty-display "Passed!"))
      
      (define model 
        (timeout
         time-limit
         (synthesize 
          #:forall sym-vars
          #:init inputs
          #:assume (if assume-interpret (interpret-spec!) (assume start-state assumption))
          #:guarantee (compare-spec-sketch))
         )
        )
      
      (pretty-display ">>> done synthesize")
      (define final-program (decode-sym sketch model))
      (define final-cost (evaluate sketch-cost model))
      
      (pretty-display ">>> superoptimize-output")
      (print-struct final-program)
      (pretty-display (format "limit cost = ~a" cost))
      (pretty-display (format "new cost = ~a" final-cost))
      (pretty-display "=====================================")
      (clear-asserts)
      ;(clear-terms!)
      (values final-program final-cost)
      )

    ;; Returns a counterexample if spec and program are different.
    ;; Otherwise, returns false.
    (define (counterexample spec program constraint [extra #f]
                            #:assume [assumption (send machine no-assumption)])
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
      
      ;; (current-solver (new kodkod%))
      (clear-asserts)
      (configure [bitwidth bit] [loop-bound 20])
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
        (assert-output spec-state program-state constraint)
        ;;(pretty-display "done check output")
        )

      (with-handlers* 
       ([exn:fail? 
         (lambda (e)
           (when debug (pretty-display "program-eq? SAME"))
           (clear-terms!)
           (if (equal? (exn-message e) "verify: no counterexample found")
               #f
               (raise e)))])
       (let ([model (verify #:assume (interpret-spec!) #:guarantee (compare))])
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
      (define in-state (send machine get-state sym-input extra))
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
      
    
    ))
