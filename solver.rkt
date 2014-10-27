#lang s-exp rosette

(require  "ast.rkt" "machine.rkt" "printer.rkt" "stat.rkt")

(require rosette/solver/smt/z3)
(require rosette/solver/kodkod/kodkod)

(provide solver%)

(struct exn:restart exn (program))

(define solver%
  (class object%
    (super-new)
    (init-field machine printer [syn-mode #f] [parser #f]
                [simulator #f] 
		[stat (new stat% [printer printer])]
                [bit (get-field bit machine)]
                [random-input-bit (get-field random-input-bit machine)])
    (abstract get-sym-vars evaluate-state
              assume assert-output len-limit window-size)
    (public proper-machine-config generate-input-states
            superoptimize superoptimize-binary superoptimize-linear
            synthesize-from-sketch counterexample
            sym-op sym-arg 
            evaluate-inst encode-sym-inst encode-sym
            assume-relax get-live-in)
    
    (if syn-mode
        (current-solver (new kodkod%))
        (current-solver (new z3%)))

    (define-syntax-rule (print-struct x) (send printer print-struct x))
    (define-syntax-rule (print-syntax x) (send printer print-syntax x))
    (define-syntax-rule (decode x) (send printer decode x))
    (define-syntax-rule (display-state x) (send machine display-state x))

    (define ninsts (vector-length (get-field inst-id machine)))
    (define start-time #f)

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

    (define (evaluate-inst x model)
      (inst (evaluate (inst-op x) model)
            (evaluate (inst-args x) model)))

    (define (evaluate-program code model)
      (traverse code inst? (lambda (x) (evaluate-inst x model))))

    (define (encode-sym-inst x)
      (if (inst-op x)
          (send printer encode-inst x)
          (inst (sym-op) (sym-arg))))

    (define (encode-sym code)
      (traverse code inst? encode-sym-inst))

    (define (sym-insts size)
      (encode-sym (for/vector ([i size]) (inst #f #f))))

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
        ;; (current-solver (new kodkod%))
        (clear-asserts)
        ;(configure [bitwidth bit] [loop-bound 20])
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
    
    (define (generate-inputs-inner n spec start-state assumption)
      (when debug
            (pretty-display `(generate-inputs-inner ,n ,assumption ,random-input-bit)))
      ;; (print-struct spec)
      ;; (display-state start-state)
      ;; (current-solver (new kodkod%))
      (clear-asserts)
      ;(configure [bitwidth bit] [loop-bound 20])
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

    (define (generate-input-states n spec assumption [extra #f])
      (define start-state (send machine get-state sym-input extra))
      (define-values (sym-vars sltns)
        (generate-inputs-inner n spec start-state assumption))
      (map (lambda (x) (evaluate-state start-state x)) sltns))

    (define (superoptimize spec constraint live-in name time-limit size [extra #f]
                           #:prefix [prefix (vector)] #:postfix [postfix (vector)]
			   #:assume [assumption (send machine no-assumption)]
			   #:input-file [input-file #f]
			   #:start-prog [start #f])
      (send stat set-name name)
      (set-field! best-correct-cost stat (send simulator performance-cost spec))
      (set! start-time (current-seconds))
      (timeout
       time-limit
       (cond
	[(equal? syn-mode `binary) 
	 (superoptimize-binary spec constraint time-limit size extra
                               #:prefix prefix #:postfix postfix
			       #:assume assumption)]

	[(equal? syn-mode `linear) 
	 (superoptimize-linear spec constraint time-limit size extra
                               #:prefix prefix #:postfix postfix
			       #:assume assumption)]

	;; [(equal? syn-mode `hybrid)
	;;  (superoptimize-binary spec constraint time-limit size extra
        ;;                        #:prefix prefix #:postfix postfix
	;; 		       #:hard-cap (len-limit)
	;; 		       #:assume assumption)]

	[(equal? syn-mode `partial1)
	 (superoptimize-partial-pattern spec constraint time-limit size extra
                                        #:hard-prefix prefix #:hard-postfix postfix
                                        #:assume assumption)]

	[(equal? syn-mode `partial2)
	 (superoptimize-partial-pattern-slow spec constraint time-limit size extra
                                             #:hard-prefix prefix #:hard-postfix postfix
                                             #:assume assumption)]

	[(equal? syn-mode `partial3)
	 (superoptimize-partial-random spec constraint time-limit size extra
                                        #:hard-prefix prefix #:hard-postfix postfix
                                        #:assume assumption)]
        )
       )
      )
	

    ;; Optimize the cost using binary search on the number of holes.
    ;; spec: non-encoded block
    (define (superoptimize-binary spec constraint time-limit size [extra #f]
				  #:lower-bound [lower-bound 0]
                                  #:assume [assumption (send machine no-assumption)]
                                  #:prefix [prefix (vector)] #:postfix [postfix (vector)]
                                  #:hard-prefix [hard-prefix (vector)] 
                                  #:hard-postfix [hard-postfix (vector)]
                                  )
      (pretty-display (format ">> superoptimize-binary"))
      (when (> (vector-length prefix) 0)
            (display "[")
            (send printer print-syntax (send printer decode prefix))
            (display "] "))
      (send printer print-syntax (send printer decode spec))
      (when (> (vector-length postfix) 0)
            (display " [")
            (send printer print-syntax (send printer decode postfix))
            (display "]"))
      (newline)
      (define prefix-len (vector-length prefix))
      (define postfix-len (vector-length postfix))

      (define final-program #f)
      (define final-len (if size size (vector-length spec)))
      (define final-cost #f)
      (define (inner begin end cost [middle (quotient (+ begin end) 2)])
	(newline)
        (pretty-display `(binary-search ,begin ,end ,middle ,cost))
        (define sketch (sym-insts middle))
        
        (define-values (out-program out-cost)
          (with-handlers* 
           ([exn:fail? 
             (lambda (e) 
               (pretty-display "catch error")
               (if (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
                       (regexp-match #rx"assert: cost" (exn-message e)))
                   (values #f cost)
                   (begin
                    (pretty-display (exn-message e))
                    (raise e))))])
           (synthesize-from-sketch (vector-append prefix spec postfix)
                                   (vector-append prefix sketch postfix)
                                   constraint extra cost time-limit
                                   #:hard-prefix hard-prefix #:hard-postfix hard-postfix
                                   #:assume assumption)))

        (when out-program 
              (set! final-program 
                    (vector-copy out-program 
                                 prefix-len 
                                 (- (vector-length out-program) postfix-len)))
              (set! final-len middle)
              (set! final-cost out-cost))

        (if out-program
            (inner begin middle out-cost)
            (when (< middle end) (inner (add1 middle) end cost))))
      
      (with-handlers 
       ([exn:break? (lambda (e) (unless final-program (set! final-program "timeout")))])
       (inner (max 1 lower-bound) final-len 
              (send simulator performance-cost (vector-append prefix spec postfix))
              (max lower-bound (quotient (+ 1 final-len) 2))))

      ;; Try len + 2
      ;; (unless (equal? final-program "timeout")
      ;;         (with-handlers 
      ;;          ([exn:break? (lambda (e) (void))])
      ;;          (inner (+ final-len 2) (+ final-len 2) final-cost)))
      

      (pretty-display "after inner")
      final-program)

    (define (superoptimize-linear spec constraint time-limit size [extra #f]
			   #:assume [assumption (send machine no-assumption)]
                           #:prefix [prefix (vector)] #:postfix [postfix (vector)]
                           #:hard-prefix [hard-prefix (vector)] #:hard-postfix [hard-postfix (vector)]
                           )
      (newline)
      (pretty-display (format ">> superoptimize-linear size = ~a" size))
      (when (> (vector-length prefix) 0)
            (display "[")
            (send printer print-syntax (send printer decode prefix))
            (display "] "))
      (send printer print-syntax (send printer decode spec))
      (when (> (vector-length postfix) 0)
            (display " [")
            (send printer print-syntax (send printer decode postfix))
            (display "]"))
      (newline)
      (define prefix-len (vector-length prefix))
      (define postfix-len (vector-length postfix))
      (define sketch (sym-insts (if size (min size (vector-length spec)) 
				    (vector-length spec))))
      (define final-program #f) ;; not including prefix & poster
      (define t #f)
      (define (inner cost)
	(newline)
        (pretty-display `(linear-inner ,(vector-length sketch) ,cost))
        (set! t (current-seconds))
	(define-values (out-program out-cost) 
	  (synthesize-from-sketch (vector-append prefix spec postfix)
                                  (vector-append prefix sketch postfix)
                                  constraint extra cost time-limit
                                  #:hard-prefix hard-prefix #:hard-postfix hard-postfix
				  #:assume assumption))
        (pretty-display `(time ,(- (current-seconds) t)))

	(set! final-program (vector-copy out-program 
                                         prefix-len 
                                         (- (vector-length out-program) postfix-len)))
	(set! sketch (vector-take sketch (vector-length final-program)))
	(inner out-cost))
      
      (with-handlers* 
       ([exn:fail? 
	 (lambda (e) 
	   (clear-terms!)
           (pretty-display "FAIL!")
           (pretty-display `(time ,(- (current-seconds) t)))
           (pretty-display (exn-message e))
	   (if (or (regexp-match #rx"synthesize: synthesis failed" (exn-message e))
                   (regexp-match #rx"assert: cost" (exn-message e))
		   (regexp-match #rx"assert: progstate-cost" (exn-message e)))
	       final-program
	       (raise e)))]
	[exn:break? (lambda (e) 
		      (clear-terms!)
                      (pretty-display "TIMEOUT!")
		      (if final-program
			   final-program
			  "timeout"))])
       (inner (send simulator performance-cost (vector-append prefix spec postfix)))))

    (define (superoptimize-partial-pattern 
             spec constraint time-limit size [extra #f]
             #:hard-prefix [hard-prefix (vector)]
             #:hard-postfix [hard-postfix (vector)]
             #:assume [assumption (send machine no-assumption)])

      (define (inner)
	(newline)
	(pretty-display "Phase 1: fixed window")
        (define program1
          (fixed-window hard-prefix hard-postfix spec constraint 60 extra assumption 
                        (window-size) (len-limit)))
        (check-global spec #f)
	;;(define program1 spec)

	(define (loop timeout w)
	  (newline)
	  (pretty-display (format "Phase 2: sliding window, timeout = ~a, window-size = ~a" 
				  timeout w))
	  (define program2
	    (sliding-window hard-prefix hard-postfix program1 
                            constraint timeout extra assumption w))
	  (check-global spec program2)
	  (loop (* 2 timeout) (add1 w)))
	(loop 60 (window-size))
        )
        
      (with-handlers*
       ([exn:restart?
         (lambda (e)
	   (superoptimize-partial-pattern 
            (exn:restart-program e)
            constraint time-limit size extra 
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix
            #:assume assumption))])
       (inner))
      )

    (define (superoptimize-partial-pattern-slow
             spec constraint time-limit size [extra #f]
             #:hard-prefix [hard-prefix (vector)]
             #:hard-postfix [hard-postfix (vector)]
             #:assume [assumption (send machine no-assumption)])
      (pretty-display "superoptimize-partial-pattern-slow")
      (define (loop timeout w)
        (newline)
        (pretty-display (format "Phase: sliding window, timeout = ~a, window-size = ~a" 
                                timeout w))
        (define program
          (sliding-window hard-prefix hard-postfix spec
                          constraint timeout extra assumption w 
			  #:restart #t #:lower-bound (add1 (len-limit))))
        (check-global spec program)
        (loop (* 2 timeout) (add1 w)))
        
      (with-handlers*
       ([exn:restart?
         (lambda (e)
	   (superoptimize-partial-pattern-slow
            (exn:restart-program e)
            constraint time-limit size extra 
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix
            #:assume assumption))])
       (loop 800 (floor (* (/ 5 4) (window-size)))))
      )

    (define (superoptimize-partial-random 
             spec constraint time-limit size [extra #f]
             #:hard-prefix [hard-prefix (vector)]
             #:hard-postfix [hard-postfix (vector)]
             #:assume [assumption (send machine no-assumption)])
      
      (define (inner w timeout choices)
        (define from (list-ref choices (random (length choices))))
        (pretty-display (format ">> superoptimize-partial-random pos = ~a, timeout = ~a" from timeout))
        (pretty-display `(choices ,choices))
        (define prefix (vector-copy spec 0 from))
        (define after-prefix (vector-copy spec from))
        (define-values (new-seq pos)
          (sliding-window-at hard-prefix hard-postfix 
                             prefix after-prefix
                             constraint timeout extra assumption w))
        (define output 
          (if new-seq
              (vector-append prefix new-seq (vector-copy after-prefix pos))
              spec))

        (check-global spec output)

        (define new-choices (remove from choices))
        (if (empty? new-choices)
            (inner (add1 w) (* 2 timeout) (range (sub1 (vector-length spec))))
            (inner w timeout new-choices))
        )

      (with-handlers*
       ([exn:restart?
         (lambda (e)
           (superoptimize-partial-random 
            (exn:restart-program e)
            constraint time-limit size extra 
            #:hard-prefix hard-prefix #:hard-postfix hard-postfix
            #:assume assumption))])
       (inner (window-size) 60 (range (sub1 (vector-length spec))))))

    (define (check-global input-prog quick-restart)
      (define-values (cost len time id) (send stat get-best-info-stat))
      (pretty-display `(check-global ,cost ,len ,id))
      (define old-cost (send simulator performance-cost input-prog))
      (define best-cost (if cost cost (get-field best-correct-cost stat)))

      (when (< best-cost old-cost)
        (when (< best-cost (get-field best-correct-cost stat))
          (pretty-display "Steal program from other."))
        (when (or quick-restart (< best-cost (get-field best-correct-cost stat)))
          (define best-program 
            (if cost 
                (send printer encode
                      (send parser ast-from-file 
                            (format "~a/best.s" (get-field dir stat))))
                quick-restart))
          (pretty-display "restart!!!!!")
          (raise (exn:restart "restart" (current-continuation-marks) best-program)))))
    
    (define (fixed-window hard-prefix hard-postfix spec constraint time-limit extra assume
                          window size-limit)
      (define len (vector-length spec))
      (define output (vector))
      (define steps (quotient len window))
      (for ([i steps])
           (let* ([start (* i window)]
                  [end (* (add1 i) window)]
                  [seq (vector-copy spec start end)]
                  [new-seq
                   (superoptimize-linear 
                    seq constraint time-limit size-limit extra #:assume assume
                    #:hard-prefix hard-prefix #:hard-postfix hard-postfix
                    #:prefix output
                    #:postfix (vector-copy spec end len))])
             (if (or (equal? new-seq #f) (equal? new-seq "timeout"))
                 (set! output (vector-append output seq))
                 (set! output (vector-append output new-seq)))))
      (set! output (vector-append output (vector-copy spec (* steps window) len)))
      (when (> len (* steps window))
            (let* ([out-len (vector-length output)]
                   [seq (vector-copy output (max 0 (- out-len window)) out-len)]
                   [prefix (vector-copy output 0 (max 0 (- out-len window)))]
                   [new-seq
                    (superoptimize-linear 
                     seq constraint time-limit size-limit extra #:assume assume
                     #:hard-prefix hard-prefix #:hard-postfix hard-postfix
                     #:prefix prefix)])
             (if (or (equal? new-seq #f) (equal? new-seq "timeout"))
                 (set! output (vector-append prefix seq))
                 (set! output (vector-append prefix new-seq)))))
      ;; (print-syntax (decode output))
      output)

    (define (sliding-window-at hard-prefix hard-postfix prefix code 
                               constraint time-limit extra assume window
			       #:lower-bound [lower-bound 0]
                               #:restart [restart #f])
      (define spec (vector-append prefix code))
      (define len-code (vector-length code))
      (define (inner pos-to)
        (define out-program
          (superoptimize-binary 
           (vector-take code pos-to) constraint time-limit #f extra #:assume assume
           #:hard-prefix hard-prefix #:hard-postfix hard-postfix
           #:prefix prefix
           #:postfix (vector-drop code pos-to)
	   #:lower-bound lower-bound))
        (when restart (check-global spec #f))
        (cond
         [(equal? out-program "timeout")
          (if (> pos-to (max 2 lower-bound)) 
              (begin
                (pretty-display "timeout => shrink")
                (inner (sub1 pos-to)))
              (values #f #f))]
         [(equal? out-program #f)
          (values #f pos-to)]
         [else
          (values out-program pos-to)]))
                  
      (inner (min len-code window)))

    (define (sliding-window hard-prefix hard-postfix spec 
			    constraint time-limit extra assume window 
			    #:restart [restart #f]
			    #:lower-bound [lower-bound 0])
      (define output (vector))
      (define (loop code)
        (when (> (vector-length code) 0)
          (define-values 
            (out-program next-pos)
            (sliding-window-at hard-prefix hard-postfix output code 
			       constraint time-limit extra assume window
                               #:restart restart #:lower-bound lower-bound
                               ))
	  (cond
	   [out-program
	    (pretty-display "found => skip")
	    (set! output (vector-append output out-program))
	    (loop (vector-drop code next-pos))]
	   [(and next-pos (>= next-pos (vector-length code)))
	    (set! output (vector-append output code))]
	   [else
	    (set! output (vector-append output (vector (vector-ref code 0))))
	    (loop (vector-drop code 1))])))
	   
      (loop spec)
      output)

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
                                    #:hard-prefix [hard-prefix (vector)] 
                                    #:hard-postfix [hard-postfix (vector)]
				    #:assume-interpret [assume-interpret #t]
				    #:assume [assumption (send machine no-assumption)])
      (pretty-display (format "SUPERPOTIMIZE: assume-interpret = ~a" assume-interpret))
      (when debug
            (send printer print-struct hard-prefix)
	    (newline)
            (send printer print-struct spec)
	    (newline)
            (send printer print-struct hard-postfix)
	    (newline)
	    )

      ;; (current-solver (new z3%))
      ;; (current-solver (new kodkod%))

      (clear-asserts)
      ;(configure [bitwidth bit] [loop-bound 20])
      (current-bitwidth bit)

      (define start-state (send machine get-state sym-input extra))
      (define spec-state #f)
      (define sketch-state #f)
      (define spec-cost #f)
      (define sketch-cost #f)
      ;; (pretty-display "========= start state")
      ;; (send machine display-state start-state)
      
      (define (interpret-spec!)
        (when debug (pretty-display "========== interpret spec"))
        (set! spec-state 
              (interpret-spec (vector-append hard-prefix spec hard-postfix)
                              start-state assumption)))
      
      (define (compare-spec-sketch)
        (when debug (pretty-display "=========== interpret sketch"))
        (set! sketch-state 
              (send simulator interpret (vector-append hard-prefix sketch hard-postfix)
                    start-state spec-state))
        (when debug (pretty-display "check output"))
        ;; (set! spec-cost (send simulator performance-cost spec))
        (set! sketch-cost (send simulator performance-cost sketch))
        (when cost (assert (< sketch-cost cost) "cost"))
        (assert-output spec-state sketch-state constraint)
        )
      
      ;; Collect input variables and contruct their init values.
      (define-values (sym-vars inputs)
        (generate-inputs-inner 3 spec start-state assumption))

      (when debug
            (pretty-display "Test calculate performance-cost with symbolic instructions...")
            (send simulator performance-cost sketch)
            (pretty-display "Test simulate with symbolic instructions...")
            (send simulator interpret sketch start-state)
            (pretty-display "Passed!"))
      
      (define model 
        (timeout
         time-limit
         (synthesize 
          #:forall sym-vars
          #:init (drop inputs 1)
          #:assume (if assume-interpret (interpret-spec!) (assume start-state assumption))
          #:guarantee (compare-spec-sketch))
         )
        )
      
      (when debug (pretty-display ">>> done synthesize"))
      (define final-program (evaluate-program sketch model))
      (define final-cost (evaluate sketch-cost model))
      
      (pretty-display ">>> superoptimize-output")
      (set! final-program (send machine clean-code final-program hard-prefix))
      ;;(send printer print-struct final-program)
      (print-syntax (decode final-program)) (newline)
      (pretty-display (format "limit cost = ~a" cost))
      (pretty-display (format "new cost = ~a" final-cost))
      (pretty-display "=====================================")
      (clear-asserts)
      ;(clear-terms!)

      ;; Print to file
      (send stat update-best-correct final-program final-cost)

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
      
      ;;(current-solver (new z3%))
      (current-log-handler (log-handler #:info any/c))
      (clear-asserts)
      ;(configure [bitwidth bit] [loop-bound 20])
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
      
    
    ))
