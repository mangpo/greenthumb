#lang racket

(require "ast.rkt" "machine.rkt" "decomposer.rkt" "arm/arm-psql.rkt")
(require racket/generator)

(provide enumerative%)

(struct concat (collection inst))

;(struct entry (live vreg))
(define-syntax-rule (entry live vreg) (cons live vreg))
(define-syntax-rule (entry-live x) (car x))
(define-syntax-rule (entry-vreg x) (cdr x))

(define enumerative%
  (class decomposer%
    (super-new)
    (init-field [generate-inst #f])
    (inherit-field machine printer validator simulator)
    (override synthesize-window)
    (abstract reset-generate-inst abstract lexical-skeleton)
    (public get-register-mapping get-renaming-iterator build-db
	    lexical-cmp)

    (define bit (get-field bit machine))

    (define (synthesize-window spec sketch prefix postfix constraint extra 
                               [cost #f] [time-limit 3600]
                               #:hard-prefix [hard-prefix (vector)] 
                               #:hard-postfix [hard-postfix (vector)]
                               #:assume-interpret [assume-interpret #t]
                               #:assume [assumption (send machine no-assumption)])
      (define spec-len (vector-length spec))
      (define org-nregs (send machine get-nregs)) ;; #f is default
      (send machine analyze-args prefix spec postfix #:vreg spec-len)
      (send machine analyze-opcode prefix spec postfix)

      (define live2 (send validator get-live-in postfix constraint extra))
      (define live2-vec (send machine progstate->vector live2))
      (define live1 (send validator get-live-in spec live2 extra))


      (define ntests 2)
      (define inits
	(send validator generate-input-states ntests (vector-append prefix spec postfix)
              assumption extra))

      (define prev-classes (make-hash))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states1-vec 
	(map (lambda (x) (send machine progstate->vector x)) states1))
      (define states2-vec-spec
	(map (lambda (x) 
	       (send machine progstate->vector 
		     (send simulator interpret spec x #:dep #f))) states1))
      (pretty-display `(states1-vec ,states1-vec))
      (pretty-display `(states2-vec-spec ,states2-vec-spec))
      (pretty-display `(live2-vec ,live2-vec))
      ;; key = (cons list of output-vec, liveout-vec)
      (hash-set! prev-classes 
		 (entry (send machine get-operand-live live1) org-nregs)
		 (make-hash (list (cons states1-vec (list (vector))))))

      (for ([i states1])
	   (send machine display-state i))

      (define t-start (current-seconds))
      (define t-refine 0)
      (define t-abst 0)
      (define t0 (current-seconds))
      (define count 0)
      (define count-1 0)
      (define candidate-gen
	(generator
	 ()
	 (define (loop iter)
	   (newline)
	   (pretty-display `(loop ,iter))
	   (define classes (make-hash))
	   (define ce-list (list))

	   (define (check-eqv prog states-vec)
	     ;(pretty-display `(check-eqv ,prog ,states-vec))
	     (set! count (add1 count))
	     (when (= (modulo count 100000) 0)
		   (define t1 (current-seconds))
		   (pretty-display `(time ,(- t1 t0) ,count 
					  ,(exact->inexact (/ count-1 count))
					  ,(exact->inexact (/ t-refine 1000 (- t1 t-start)))))
		   ;; (pretty-display `(count/sec ,(exact->inexact (/ count (- t1 t0)))))
		   ;; (set! count 0)
		   (set! t0 t1))

	     ;; (when (concat? prog) 
	     ;; 	   (let ([x (concat-inst prog)])
	     ;; 	     (when (and (equal? `rsb
             ;;                            (vector-ref (get-field inst-id machine) 
             ;;                                        (inst-op x)))
	     ;; 			(equal? 4 (vector-ref (inst-args x) 1))
	     ;; 			(equal? 0 (vector-ref (inst-args x) 2)))
             ;;               (newline)
	     ;; 		   (pretty-display `(states-vec ,live2-vec
	     ;; 						,states-vec
	     ;; 						,states2-vec-spec))
             ;;               (newline))))

	     (when
	      (for/and ([state-spec states2-vec-spec]
			[state states-vec])
		       (send machine relaxed-state-eq? state-spec state live2-vec))
	      (set! count-1 (add1 count-1))
	      (define t-refine-start (current-milliseconds))
	      (when debug (pretty-display "[1] correct on first query"))

	      (define (inner-loop iterator)
		(define p (iterator))
		(when p
		      ;; (pretty-display "After renaming")
		      ;; (send printer print-syntax (send printer decode p))
		      (when
		       (for/and ([input-output ce-list])
				(let* ([input (car input-output)]
				       [output-vec (cdr input-output)]
				       [my-output-vec
					(with-handlers*
					 ([exn? (lambda (e) #f)])
					 (send machine progstate->vector
					       (send simulator interpret p input #:dep #f)))])
				  (and my-output-vec
				       (send machine state-eq? output-vec my-output-vec live2-vec))))
		       (when debug
			     (pretty-display "[2] all correct")
			     (pretty-display `(ce-list ,(length ce-list)))
			     (when (= (length ce-list) 100)
				   (for ([i 10]
					 [ce ce-list])
					(send machine display-state (car ce)))
				   (send printer print-syntax (send printer decode p))
				   (raise "done")
				   )
			     )
		       
		       (define ce (send validator counterexample 
					(vector-append prefix spec postfix)
					(vector-append prefix p postfix)
					constraint extra #:assume assumption))

		       (if ce
			   (let* ([ce-input (send simulator interpret prefix ce #:dep #f)]
				  [ce-output-vec
				  (send machine progstate->vector
					(send simulator interpret spec ce-input #:dep #f))])
			     (pretty-display "[3] counterexample")
			     (set! ce-list (cons (cons ce-input ce-output-vec) ce-list)))
			   (begin
			     (pretty-display "[4] FOUND!!!")
			     (pretty-display 
			      `(time ,count 
				     ,(exact->inexact (/ count-1 count))
				     ,(exact->inexact (/ t-refine 1000 
							 (- (current-seconds) t-start)))
				     ,(exact->inexact (/ t-abst 1000 
							 (- (current-seconds) t-start)))))
			     (let ([groups (hash-keys classes)])
			       (send printer print-syntax (send printer decode p))
			       (pretty-display `(groups ,(length groups))))
			     (yield p)))
		       )
		      (inner-loop iterator)))

	      (define mapping 
		(get-register-mapping org-nregs states2-vec-spec states-vec live2-vec))

	      (define (loop iterator)
		(define p (iterator))
		(when p 
		      ;; (newline)
		      ;; (pretty-display "Before renaming")
		      ;; (send printer print-syntax (send printer decode p))
		      (when mapping
			    (define iterator2 (get-renaming-iterator p mapping))
			    (inner-loop iterator2)
			    )
		      (loop iterator))
		)

	      (when mapping (loop (get-collection-iterator prog)))
	      (set! t-refine (+ t-refine (- (current-milliseconds) t-refine-start)))
	      )
	     ) ;; End check-eqv

	   (define (refine-real classes live-list my-vreg my-inst)
	     (for ([states classes])
		  (let ([states2-vec 
			 (with-handlers*
			  ([exn? (lambda (e) #f)])
			  (map (lambda (x) 
				 (send machine progstate->vector 
				       (send simulator interpret (vector my-inst)
					     (send machine vector->progstate x)
					     #:dep #f))) 
			       states))])
		    (when states2-vec
			  (check-eqv (concat (class-ref prev-classes live-list my-vreg states) 
					     my-inst)
				     states2-vec)))))

	   (define (refine-abstract classes live-list my-vreg new-live-list my-inst k type)
	     (define f
	       (cond
		[(equal? type `mod) 
		 (let ([base (arithmetic-shift 1 k)])
		   (lambda (x) (modulo x base)))]
		[(equal? type `high) 
		 (let ([mask (arithmetic-shift -1 (- bit k))])
		   (lambda (x) (bitwise-and x mask)))]))
	     (define abst-hash classes)
	     (define abst-expect (map (lambda (x) (abstract x live-list f)) states2-vec-spec))
	     (when (list? abst-hash)
		   ;; TODO: live-list should contain memory as well
		   (set! abst-hash (make-hash))
		   (for ([states classes])
			(let ([abst-states (map (lambda (x) (abstract x live-list f)) states)])
			  (if (hash-has-key? abst-hash abst-states)
			      (hash-set! abst-hash abst-states
					 (cons states (hash-ref abst-hash abst-states)))
			      (hash-set! abst-hash abst-states (list states)))))
		   
		   ;; (pretty-display `(live-list ,live-list))
		   ;; (pretty-display `(abst-hash ,(hash-count abst-hash)))
		   )

	     (for ([pair (hash->list abst-hash)])
		  (let* ([abst-states (car pair)]
			 [real-states (cdr pair)]
			 [abst-states-out 
			  (with-handlers*
			   ([exn? (lambda (e) #f)])
			   (map (lambda (x) 
				  (abstract 
				   (send machine progstate->vector 
					 (send simulator interpret (vector my-inst)
					       (send machine vector->progstate x) 
					       #:dep #f))
				   new-live-list f))
				abst-states))])
		    	 ;; [condition 
		    	 ;;  (and abst-states-out
		    	 ;;  (for/and ([state-spec abst-expect]
		    	 ;; 	    [state abst-states-out])
		    	 ;; 	   (send machine relaxed-state-eq? state-spec state live2-vec)))])
		    ;; (when (and (equal? `and 
		    ;; 		       (vector-ref (get-field inst-id machine) (inst-op my-inst)))
		    ;; 	       (equal? 2 (vector-ref (inst-args my-inst) 0))
		    ;; 	       (equal? 0 (vector-ref (inst-args my-inst) 1))
		    ;; 	       (equal? 1 (vector-ref (inst-args my-inst) 2)))
		    ;; 	  (pretty-display `(info ,k
		    ;; 				 ,abst-states
		    ;; 				 ,abst-states-out
		    ;; 				 ,abst-expect
		    ;; 				 ,live2-vec
		    ;; 				 ,condition)))
		    (when 
		     (and abst-states-out
			  (for/and ([state-spec abst-expect]
				    [state abst-states-out])
				   (send machine relaxed-state-eq? state-spec state live2-vec)))
		     (if (< k 6)
			 (hash-set! 
			  abst-hash
			  abst-states
			  (refine-abstract real-states live-list my-vreg new-live-list my-inst 
					   (add1 k) type))
			 (refine-real real-states live-list my-vreg my-inst))
		     )))

	     abst-hash
	     )
	     

	   ;; Enmerate all possible program of one instruction
	   (define (enumerate states progs-collection check) 
	     (define (inner)
               (when debug (pretty-display `(inner1)))
	       ;; Call instruction generator
	       (define inst-liveout-vreg (generate-inst))
               (when debug (pretty-display `(inner2)))
	       (define my-inst (first inst-liveout-vreg))
	       (define my-liveout (second inst-liveout-vreg))
	       (define my-vreg (third inst-liveout-vreg))
               (when debug (pretty-display `(inner ,inst-liveout-vreg)))
               (when 
                my-inst
                
		(when debug
		      (send printer print-syntax-inst (send printer decode-inst my-inst))) 
		(let ([states2-vec 
		       (with-handlers*
			([exn? (lambda (e) #f)])
			(map (lambda (x) 
			       (send machine progstate->vector 
				     (send simulator interpret (vector my-inst)
					   x #:dep #f))) states))]
		      [prog (concat progs-collection my-inst)])
		  (when debug (pretty-display `(after-interpret ,(list? states2-vec))))
		  
		  (when states2-vec 
			(class-insert! classes my-liveout states2-vec my-vreg prog)
			(when check (check-eqv prog states2-vec))
			))
		(inner)))
	     (inner))
	   
	   (pretty-display `(eqv ,(hash-count prev-classes)))
	   ;; Test
	   (define t-abst-start (current-milliseconds))
	   (define (abst-loop eqv-classes live-list my-vreg type)
	     (define inst-liveout-vreg (generate-inst)) ;; TODO: close under modulo
	     (define my-inst (first inst-liveout-vreg))
	     (define my-liveout (second inst-liveout-vreg))
	     (when my-inst
		   ;;(send printer print-syntax (send printer decode my-inst))
	   	   (define abst-hash 
	   	     (refine-abstract eqv-classes live-list my-vreg my-liveout my-inst 1 type))
	   	   (abst-loop abst-hash live-list my-vreg type)))

	   (for ([pair1 (hash->list prev-classes)])
	   	(let* ([live-vreg (car pair1)]
	   	       [live-list (entry-live live-vreg)]
	   	       [my-vreg (entry-vreg live-vreg)]
	   	       [hash2 (cdr pair1)]
	   	       [eqv-classes (hash-keys hash2)]
		       ;; use only first state
		       [state-rep-list (list (send machine vector->progstate (caar eqv-classes)))])
		  ;; modular abstraction
	   	  (reset-generate-inst state-rep-list live-list my-vreg `mod #f) 
	   	  (abst-loop  eqv-classes live-list my-vreg `mod)
		  ;; high-byte-mask abstraction
	   	  (reset-generate-inst state-rep-list live-list my-vreg `high #f)
	   	  (abst-loop  eqv-classes live-list my-vreg `high)
	   	  ))

	   (set! t-abst (+ t-abst (- (current-milliseconds) t-abst-start)))
	   (pretty-display `(abstract-done))

	   ;; Grow
	   (for ([pair1 (hash->list prev-classes)])
		(let* ([live-vreg (car pair1)]
		       [live-list (entry-live live-vreg)]
		       [my-vreg (entry-vreg live-vreg)]
		       [hash2 (cdr pair1)])
		  (for ([pair2 (hash->list hash2)])
		       (let* ([val (cdr pair2)]
			      [outputs (map (lambda (x) (send machine vector->progstate x)) 
					    (car pair2))]
			      [smallest-lex (get-smallest-lex val)]
			      )
			 ;; Initialize enumeration one instruction process
			 (when debug
			       (pretty-display `(ENUM!!!!!!!!!!!!! ,val))
			       (print-concat val)
			       )
			 (reset-generate-inst outputs live-list my-vreg `rest smallest-lex)
			 (enumerate outputs val #t) ;; check
			 (reset-generate-inst outputs live-list my-vreg `mod smallest-lex)
			 (enumerate outputs val #f) ;; no check
			 (reset-generate-inst outputs live-list my-vreg `high smallest-lex)
			 (enumerate outputs val #f) ;; no check
			 ))))
	   (when (< iter spec-len)
		 (pretty-display `(iter ,iter ,spec-len))
		 (set! prev-classes classes)
		 (loop (add1 iter))))
	 (loop 0)))

      (print-concat (candidate-gen))
      )

    (define (class-insert! class live-vec states-vec my-vreg prog)
      (define key (entry live-vec my-vreg))
      (if (hash-has-key? class key)
	  (let ([hash2 (hash-ref class key)])
	    (if (hash-has-key? hash2 states-vec)
		(let ([val (hash-ref hash2 states-vec)])
		  (hash-set! hash2 states-vec (cons prog val)))
		(hash-set! hash2 states-vec (list prog))))
	  (hash-set! class key 
		     (make-hash (list (cons states-vec (list prog)))))))

    (define (class-ref class live-vec my-vreg states-vec)
      ;; (pretty-display `(class-ref ,live-vec ,my-vreg ,states-vec))
      ;; (pretty-display `(level1 ,(hash-ref class (entry live-vec my-vreg))))
      (hash-ref (hash-ref class (entry live-vec my-vreg)) states-vec))

    (define (print-concat collection)
      (define (inner x [indent ""])
	(cond
	 [(concat? x)
	  (pretty-display (format "~a[concat" indent))
	  (inner (concat-collection x) (string-append indent "  "))
	  (send printer print-syntax-inst (send printer decode-inst (concat-inst x)) (string-append indent "  "))
	  ;;(send printer print-struct-inst (concat-inst x) (string-append indent "  "))
	  (pretty-display (format "~a]" indent))
	  ]

	 [(list? x)
	  (for ([i x])
	       (inner i indent))]

	 [else
	  (pretty-display (format "~a~a" indent x))])
	)
      (inner collection))

    (define (lexical-cmp x y)
      (cond
       [(number? x)
	(cond
	 [(< x y) -1]
	 [(> x y) 1]
	 [(= x y) 0])]
       
       [(list? x)
	(define (loop i j)
	  (cond
	   [(and (empty? i) (empty? j)) 0]
	   [(empty? i) -1]
	   [(empty? j) 1]
	   [else
	    (define ans (lexical-cmp (car i) (car j)))
	    (if (not (= ans 0))
		ans
		(loop (cdr i) (cdr j)))]))
	(loop x y)]))

    (define (lexical-smaller x y)
      (if (and x y) 
	  (if (= (lexical-cmp x y) -1) x y)
	  #f))
    
    (define (get-smallest-lex x)
      (define (f x)
	(cond
	 [(concat? x) (lexical-skeleton (concat-inst x))]
	 [(vector? x)
	  (define len (vector-length x))
	  (if (= len 0) #f (lexical-skeleton (vector-ref x (sub1 (vector-length)))))]
	 [(list? x) 
	  (if (empty? x)
	      #f
	      (foldl (lambda (x res) 
		       (lexical-smaller (f x) res))
		     (f (car x)) (cdr x)))]))
      (f x))

    (define (get-first-program x [postfix (vector)])
      (cond
        [(concat? x)
         (get-first-program (concat-collection x) (vector-append (vector (concat-inst x)) postfix))]
        [(vector? x) (vector-append x postfix)]
        [(list? x) 
         (if (empty? x)
             postfix
             (get-first-program (first x) postfix))]))

    (define (get-collection-iterator collection)
      (define iterate-collection
	(generator
	 ()
	 (define (loop x postfix)
	   (cond
	    [(concat? x)
	     (loop (concat-collection x) (vector-append (vector (concat-inst x)) postfix))]
	    [(vector? x) 
	     (yield (vector-append x postfix))]
	    [(list? x) 
	     (if (empty? x)
		 (yield postfix)
		 (for ([i x]) (loop i postfix)))]))
	 (loop collection (vector))
	 #f))
      iterate-collection)
     
    (define (get-register-mapping nregs states-vec-spec states-vec liveout-vec)
      #t)
     
    (define (get-renaming-iterator prog mapping)
      (generator
       ()
       (yield prog)
       (yield #f)))
      
    (define (build-db)
      (system "rm progress.log")
      (define time (new time% [total-start (current-seconds)]))
      (send machine reset-inst-pool)
      (define constraint-all (send machine constraint-all))
      (define constraint-all-vec (send machine progstate->vector constraint-all))
      (define live-list (send machine get-operand-live constraint-all))
      
      (define psql (new arm-psql% [machine machine] [printer printer] [time time]))
      (define all-states (send psql get-all-states))
      (define all-states-vec 
        (map (lambda (x) (send machine progstate->vector x)) all-states))
      
      (send psql db-connect)
      (send psql init (length all-states))
      
      (send time reset)
      (define max-size 2)
      ;; (define prev-classes (make-hash))
      ;; (hash-set! prev-classes all-states-vec (list (progcost (vector) 0)))
      (send psql create-table 0 (length all-states))
      (send psql insert 0 all-states all-states (vector))

      (define count 0)
      (define my-count 0)
      (define all-count 0)

      (define (loop len)
        (define classes (make-hash))

        (define (build-table prog perf out-states)
          (when #t
                (newline)
                (send printer print-syntax (send printer decode prog)))
	  (define unique #t)
          (send time start `vector)
	  (define key (map (lambda (x) (send machine progstate->vector x)) out-states))
          (send time end `vector)
	  (define (check-inmem x)
            (send time start `hash)
            (define match (hash-has-key? x key))
            (send time end `hash)
	    (when match
                  (send time start `hash)
	          (let ([rets (hash-ref x key)])
                    (send time end `hash)
	            (when #t (pretty-display (format "validate: ~a" (length rets))))
	            (let ([same 
                           (for/or ([ret rets])
                                   (and (send psql same? ret prog) ret))])
	              (when debug (pretty-display "validate: done"))
	              (when same
	        	    (set! unique #f)
	        	    (let ([same-perf (send simulator performance-cost same)])
	        	      (when #t (pretty-display "[not unique]"))
	        	      (when (< perf same-perf)
                                    (send time start `hash)
	        		    (hash-set! x key (remove same rets))
	        		    (if (hash-has-key? classes key)
	        			(hash-set! classes key (cons prog
	        						     (hash-ref classes key)))
	        			(hash-set! classes key (list prog)))
                                    (send time end `hash)
                                    )))))))

          (when debug (pretty-display "Check current table."))
          (check-inmem classes)
          (when unique ;; don't check with persistant memory
                (when #t (pretty-display "[unique]"))
                (set! all-count (add1 all-count))
                (send time start `hash)
                (if (hash-has-key? classes key)
                    (begin
                      (hash-set! classes key (cons prog (hash-ref classes key)))
                      (send time end `hash)
                      )
                    (begin
                      (hash-set! classes key (list prog))
                      (send time end `hash)
                      (set! my-count (add1 my-count))
                      (when (= my-count 50000)
                            ;(send time print-ce)
                            (send psql bulk-insert len classes #t)
                            (set! classes (make-hash))
                            (set! my-count 0)
                            (set! all-count 0)
                            (collect-garbage)
                            (send time reset)
                            )
                      )
                    )
                )
          )

        ;; Enmerate all possible program of one instruction
        (define (enumerate states prog-list)
          (define (inner)
            ;; Call instruction generator
            (define inst-liveout-vreg (generate-inst))
            (define my-inst (first inst-liveout-vreg))
            (define my-liveout (second inst-liveout-vreg))
            (define my-vreg (third inst-liveout-vreg))
            (when 
             my-inst
             
             (when debug
                   (send printer print-syntax-inst (send printer decode-inst my-inst))) 
             (send time start `normal-test)
             (let ([out-states 
                    (for/list ([state states])
                              (with-handlers*
                               ([exn? (lambda (e) #f)])
                               (send simulator interpret (vector my-inst) state #:dep #f)))])
               (send time end `normal-test)
               ;(pretty-display `(legal ,(for/or ([x out-states]) x)))
               (when 
                (for/or ([x out-states]) x)
                ;; If everything is false => illegal program, exclude from table
                (set! count (add1 count))
                (when (= (modulo count 1000) 0) 
                      (pretty-display `(count ,count ,my-count ,all-count ,(current-memory-use)))
                      (with-output-to-file "progress.log" #:exists 'append
                        (thunk 
                         (display (format "count: ~a, ~a, ~a, ~a\t| " 
                                          count my-count all-count (current-memory-use)))
                         (send time terminate)
                         (send time print-stat-concise)
                         (send time reset)
                         ))
                      )

                (for ([old-prog prog-list])
                     (let* ([prog (vector-append old-prog (vector my-inst))]
                            [cost (send simulator performance-cost prog)])
                       (build-table prog cost out-states)))))
             (inner)))
          (inner))
            
        (send psql create-table len (length all-states))

        (define data (send psql select-all (sub1 len)))
        (send time reset)
        (for ([x data])
             (let ([progs (record-progs x)]
                   [outputs (record-states x)])
               (reset-generate-inst outputs live-list #f `all #f)
               (enumerate outputs progs)))

        (send psql bulk-insert len classes #t)
        (set! classes (make-hash))
        (set! my-count 0)
        (set! all-count 0)
        (collect-garbage)

        (when (< len max-size) 
	    (loop (add1 len))))
      
      (with-handlers
       ([exn:break? (lambda (e) (void))])
       ;(send psql tx-begin)
       (loop 1)
       ;(send psql tx-commit)
       )
      (send time terminate)
      (send time print-stat)
      (pretty-display `(total-count ,count))

      )


    ))
