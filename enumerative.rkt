#lang racket

(require "ast.rkt" "machine.rkt" "decomposer.rkt" "arm/arm-psql.rkt")
(require racket/generator)

(provide enumerative%)

(struct concat (collection inst))
(struct entry (progs vreg))
(struct progcost (prog cost))

(define enumerative%
  (class decomposer%
    (super-new)
    (init-field [generate-inst #f])
    (inherit-field machine printer validator simulator)
    (override synthesize-window)
    (abstract reset-generate-inst)
    (public get-register-mapping get-renaming-iterator build-db)

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
		 (cons states1-vec (send machine get-operand-live live1)) 
		 (entry (list (vector)) org-nregs))

      (for ([i states1])
	   (send machine display-state i))

      (define t-start (current-seconds))
      (define t0 (current-seconds))
      (define count 0)
      (define candidate-gen
	(generator
	 ()
	 (define (loop iter)
	   (pretty-display `(loop ,iter))
	   (define classes (make-hash))
	   (define ce-list (list))

	   (define (build-table prog my-liveout-vec my-vreg liveout-vec states-vec-spec states-vec)
	     
	     (let ([key (cons states-vec my-liveout-vec)])
	       (if (hash-has-key? classes key)
		   (let ([val (hash-ref classes key)])
		     (hash-set! classes key (entry (cons prog (entry-progs val)) 
						   (max my-vreg (entry-vreg val)))))
		   (hash-set! classes key (entry (list prog) my-vreg))
                   ))
	       ;; (pretty-display `(insert-table))
	       ;; (print-concat (hash-ref classes key))

	     ;; (when (concat? prog) 
	     ;; 	   (let ([x (concat-inst prog)])
	     ;; 	     (when (and (equal? `@p 
             ;;                            (vector-ref (get-field inst-id machine) 
             ;;                                        (inst-op x)))
	     ;; 			(equal? 3 (inst-args x)))
             ;;               (newline)
	     ;; 		   (pretty-display `(states-vec ,states-vec
	     ;; 						,states-vec-spec))
             ;;               (newline))))
	     (when
	      (for/and ([state-spec states-vec-spec]
			[state states-vec])
		       ;(send machine state-eq? state-spec state liveout-vec))
		       (send machine relaxed-state-eq? state-spec state liveout-vec))
	      (when debug (pretty-display "[1] correct on first query"))

	      (define (inner-loop iterator)
		(define p (iterator))
		(when p
		      (pretty-display "After renaming")
		      (send printer print-syntax (send printer decode p))
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
				       (send machine state-eq? output-vec my-output-vec liveout-vec))))
		       (when #t 
			     (pretty-display "[2] all correct")
			     (pretty-display `(time ,(- (current-seconds) t-start)))
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
			     (let ([groups (hash-keys classes)])
			       (send printer print-syntax (send printer decode p))
			       (pretty-display `(groups ,(length groups))))
			     (yield p)))
		       )
		      (inner-loop iterator)))

	      (define mapping 
		(get-register-mapping org-nregs states-vec-spec states-vec liveout-vec))

	      (define (loop iterator)
		(define p (iterator))
		(when p 
		      (newline)
		      (pretty-display "Before renaming")
		      (send printer print-syntax (send printer decode p))
		      (when mapping
			    (define iterator2 (get-renaming-iterator p mapping))
			    ;; (pretty-display "After renaming")
			    ;; (send printer print-syntax (send printer decode (iterator2)))
			    (inner-loop iterator2)
			    )
		      (loop iterator))
		)

	      (when mapping (loop (get-collection-iterator prog)))
	     )) ;; End build table

	   ;; Enmerate all possible program of one instruction
	   (define (enumerate states progs-collection) 
	     (define (inner)
               (when debug (pretty-display `(inner1)))
	       ;; Call instruction generator
	       (define inst-liveout-vreg (generate-inst))
               (when debug (pretty-display `(inner2)))
	       (define my-inst (first inst-liveout-vreg))
	       (define my-liveout (second inst-liveout-vreg))
	       (define my-vreg (third inst-liveout-vreg))
               (when debug (pretty-display `(inner ,inst-liveout-vreg)))
	       (set! count (add1 count))
	       (when (= count 100000)
		     (define t1 (current-seconds))
		     (pretty-display `(time ,(- t1 t0) ,count))
		     (pretty-display `(count/sec ,(exact->inexact (/ count (- t1 t0)))))
		     (set! count 0)
		     (set! t0 t1))
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
			(build-table prog my-liveout my-vreg live2-vec
				     states2-vec-spec states2-vec)))
		(inner)))
	     (inner))
	   
	   (for ([key (hash-keys prev-classes)])
		(let ([val (hash-ref prev-classes key)]
		      [outputs (map (lambda (x) (send machine vector->progstate x)) (car key))]
		      [live-list (cdr key)])
		  ;;(pretty-display `(key ,(car key) ,(cdr key)))
		  ;; Initialize enumeration one instruction process
		  (reset-generate-inst outputs live-list (entry-vreg val))
		  (when debug
			(pretty-display `(ENUM!!!!!!!!!!!!! ,(entry-progs val)))
			(print-concat (entry-progs val)))
		  (enumerate outputs (entry-progs val))))
	   (when (< iter spec-len)
		 (pretty-display `(iter ,iter ,spec-len))
		 (set! prev-classes classes)
		 (loop (add1 iter))))
	 (loop 0)))

      (print-concat (candidate-gen))
      )

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
        
    ;; 1 instruction: 75 s (woonsen)
    (define (build-db2)
      (send machine reset-inst-pool)
      (define psql (new arm-psql% [machine machine] [printer printer]))
      (define all-states (send psql get-all-states))
      (send psql db-connect)
      (send psql init (length all-states))
      (send psql create-table 1 (length all-states))

      ;; Make sure z of (car all-states) is not -1
      (reset-generate-inst all-states (range (send machine get-nregs)) #f)

      (define (inner p)
        (define all-states-out 
          (for/list ([state all-states])
                    (with-handlers*
                     ([exn? (lambda (e) #f)])
                     (send simulator interpret (vector p) state #:dep #f))))
        (send psql insert 
              1 (send simulator performance-cost (vector p))
              all-states all-states-out (vector p))
        (define next (first (generate-inst)))
        (when next (inner next))
        )
      (inner (first (generate-inst)))
      (send psql db-disconnect))
      
    (define (build-db)
      (send machine reset-inst-pool)
      (define constraint-all (send machine constraint-all))
      (define constraint-all-vec (send machine progstate->vector constraint-all))
      (define live-list (send machine get-operand-live constraint-all))
      
      (define psql (new arm-psql% [machine machine] [printer printer]))
      (define all-states (send psql get-all-states))
      (define all-states-vec 
        (map (lambda (x) (send machine progstate->vector x)) all-states))
      
      (send psql db-connect)
      (send psql init (length all-states))
      (send psql create-table 1 (length all-states))
      
      (define prev-classes (make-hash))
      (hash-set! prev-classes all-states-vec (list (progcost (vector) 0)))

      (define ce-list (list))

      
      (define (same? x y)
        ;(pretty-display "same?")
        (define 
          all-correct
          (for/and ([ce ce-list])
                   (let ([x-out 
                          (with-handlers*
                           ([exn? (lambda (e) #f)])
                           (send simulator interpret x ce #:dep #f))]
                         [y-out
                          (with-handlers*
                           ([exn? (lambda (e) #f)])
                           (send simulator interpret y ce #:dep #f))])
                     (send machine display-state ce)
                     ;(pretty-display `(out ,x-out ,y-out))
                     (if
                      (and x-out y-out)
                      (let ([x-out-vec (send machine progstate->vector x-out)]
                            [y-out-vec (send machine progstate->vector y-out)])
                        (send machine state-eq? x-out-vec y-out-vec constraint-all-vec))
                      (and (not x-out) (not y-out))))))

        (when all-correct (pretty-display "CE: search"))

        (with-handlers* 
         ([exn:break? (lambda (e) (pretty-display "CE: timeout") #f)])
         (if all-correct
             (let ([ce (timeout 120 (send validator counterexample x y constraint-all #f))])
               (when all-correct (pretty-display "CE: done"))
               (if ce
                   (begin
                     (pretty-display `(ce-list ,(length ce-list)))
                     (set! ce-list (cons ce ce-list))
                     #f)
                   #t))
             #f)))
        

      (define (loop len)
        (define classes (make-hash))

        (define (build-table prog perf out-states) ;; TODO: prog is a list of programs.
          (send printer print-syntax (send printer decode prog))
          (define key
            (map (lambda (x) (send machine progstate->vector x)) out-states))
          (if (hash-has-key? classes key)
              (let ([rets (hash-ref classes key)])
                (pretty-display (format "validate: ~a" (length rets)))
                (let ([same (for/or ([ret rets])
                              (and (same? (progcost-prog ret) prog) ret))])
                  (pretty-display "validate: done")
                  (if same
                      (let ([same-perf (progcost-cost same)])
                        (pretty-display "[not unique]")
                        (when (< perf same-perf)
                          (hash-set! classes key 
                                     (cons (progcost prog perf) (remove same rets)))))
                      (let ([act (send psql check-delete len perf all-states out-states prog)])
                        (pretty-display "[unique]")
                        (when act (hash-set! classes key (cons (progcost prog perf) rets)))))))
              (let ([act (send psql check-delete len perf all-states out-states prog)])
                (when act (hash-set! classes key (list (progcost prog perf))))))
          )

        ;; Enmerate all possible program of one instruction
        (define (enumerate states prog-cost-list)
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
             (let ([out-states 
                    (for/list ([state all-states])
                              (with-handlers*
                               ([exn? (lambda (e) #f)])
                               (send simulator interpret (vector my-inst) state #:dep #f)))])
               ;(pretty-display `(legal ,(for/or ([x out-states]) x)))
               (when 
                (for/or ([x out-states]) x)
                ;; If everything is false => illegal program, exclude from table
                (for ([x prog-cost-list])
                     (let* ([old-prog (progcost-prog x)]
                            [old-cost (progcost-cost x)]
                            [prog (vector-append old-prog (vector my-inst))]
                            [cost (+ old-cost 
                                     (send simulator performance-cost (vector my-inst)))])
                       (build-table prog cost out-states)))))
             (inner)))
          (inner))
            
        
        (for ([key (hash-keys prev-classes)])
             (let ([val (hash-ref prev-classes key)]
                   [outputs (map (lambda (x) (send machine vector->progstate x)) key)])
               ;;(pretty-display `(key ,(car key) ,(cdr key)))
               ;; Initialize enumeration one instruction process
               (reset-generate-inst outputs live-list #f)
               (when debug
                     (pretty-display `(ENUM!!!!!!!!!!!!! ,val)))
               (enumerate outputs val)))
        
        ;; Add this batch of programs into persistent DB.
        (for ([key (hash-keys prev-classes)])
             (let ([outputs (map (lambda (x) (send machine vector->progstate x)) key)])
               (for ([x (hash-ref prev-classes key)])
                    (send psql insert len (progcost-cost x) all-states outputs (progcost-prog x)))))

        (set! prev-classes classes)
        (when (< len 2)
              (pretty-display `(iter ,len))
              (loop (add1 len))))
      
      (loop 1)

      )


    ))
