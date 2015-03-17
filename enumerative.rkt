#lang racket

(require "ast.rkt" "machine.rkt" "decomposer.rkt")
(require racket/generator)

(provide enumerative%)

(struct concat (collection inst))
(struct entry (input outputvec table))

(define enumerative%
  (class decomposer%
    (super-new)
    (init-field [generate-inst #f])
    (inherit-field machine printer validator simulator)
    (override synthesize-window)
    (abstract reset-generate-inst)

    (define (synthesize-window spec sketch prefix postfix constraint extra 
                               [cost #f] [time-limit 3600]
                               #:hard-prefix [hard-prefix (vector)] 
                               #:hard-postfix [hard-postfix (vector)]
                               #:assume-interpret [assume-interpret #t]
                               #:assume [assumption (send machine no-assumption)])
      (send machine analyze-args prefix spec postfix)
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
      (hash-set! prev-classes (cons states1-vec (send machine get-operand-live live1)) 
		 (list (vector)))

      (for ([i states1])
	   (send machine display-state i))

      (define t0 (current-seconds))
      (define count 0)
      (define candidate-gen
	(generator
	 ()
	 (define (loop iter)
	   (pretty-display `(loop ,iter))
	   (define classes (make-hash))
	   (define ce-list (list))

	   (define (build-table prog my-liveout-vec liveout-vec states-vec-spec states-vec)
	     
	     (let ([key (cons states-vec my-liveout-vec)])
	       (if (hash-has-key? classes key)
		   (hash-set! classes key (cons prog (hash-ref classes key)))
		   (hash-set! classes key (list prog)))
	       ;; (pretty-display `(insert-table))
	       ;; (print-concat (hash-ref classes key))
	       )

	     ;; (when (concat? prog) 
	     ;; 	   (let ([x (concat-inst prog)])
	     ;; 	     (when (and (equal? `cmp (vector-ref (get-field inst-id machine) (inst-op x)))
	     ;; 			(equal? 2 (vector-ref (inst-args x) 0))
	     ;; 			(equal? 3 (vector-ref (inst-args x) 1)))
	     ;; 		   (pretty-display `(states-vec ,states-vec)))))
	     (when
	      (for/and ([state-spec states-vec-spec]
			[state states-vec])
		       (send machine state-eq? state-spec state liveout-vec))
	      (when debug (pretty-display "[1] correct on first query"))

	      (define iterator (get-collection-iterator prog))
	      (define (loop p)
		(when p
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
		      (loop (iterator))))
	      (loop (iterator)))
	     )

	   ;; Enmerate all possible program of one instruction
	   (define (enumerate states progs-collection) 
	     (define (inner)
	       ;; Call instruction generator
	       (define inst-liveout (generate-inst))
               (when debug (pretty-display `(inner ,inst-liveout)))
	       (set! count (add1 count))
	       (when (= count 100000)
		     (define t1 (current-seconds))
		     (pretty-display `(time ,(- t1 t0) ,count))
		     (pretty-display `(count/sec ,(exact->inexact (/ count (- t1 t0)))))
		     (set! count 0)
		     (set! t0 t1))
               (when 
                inst-liveout
                
		(when debug
		      (send printer print-syntax-inst (send printer decode-inst (car inst-liveout)))) 
		(let ([states2-vec 
		       (with-handlers*
			([exn? (lambda (e) #f)])
			(map (lambda (x) 
			       (send machine progstate->vector 
				     (send simulator interpret (vector (car inst-liveout))
					   x #:dep #f))) states))]
		      [prog (concat progs-collection (car inst-liveout))])
		  (when debug (pretty-display `(after-interpret ,(list? states2-vec))))
		  
		  (when states2-vec 
			(build-table prog (cdr inst-liveout) live2-vec
				     states2-vec-spec states2-vec)))
		(inner)))
	     (inner))
	   
	   (for ([key (hash-keys prev-classes)])
		(let ([progs-collection (hash-ref prev-classes key)]
		      [outputs (map (lambda (x) (send machine vector->progstate x)) (car key))]
		      [live-list (cdr key)])
		  ;; (pretty-display `(key ,(car key) ,(cdr key)))
		  ;; Initialize enumeration one instruction process
		  (reset-generate-inst outputs live-list)
		  ;; (pretty-display `(ENUM!!!!!!!!!!!!! ,progs-collection ,(vector? progs-collection)))
		  ;; (print-concat progs-collection)
		  (enumerate outputs progs-collection)))
	   (when (< iter 2)
		 (set! prev-classes classes)
		 (loop (add1 iter))))
	 (loop 0)))

      ;; TODO: verify
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
     
    (define (build-table2 prefix spec postfix constraint extra assumption
			 classes prog my-liveout-vec liveout-vec states-vec-spec states-vec)

      (define (check-and-insert-new classes prog states-vec-spec states-vec)
	(when debug (pretty-display `(check-and-insert-new ,states-vec-spec ,states-vec)))

	(cond
	 [(for/and ([state-spec states-vec-spec]
		    [state states-vec])
		   (send machine state-eq? state-spec state liveout-vec))
	  (pretty-display "[1] all correct")
	  ;; validate equivalence
	  (define first-prog (get-first-program prog))
	  (define ce (send validator counterexample 
			   (vector-append prefix spec postfix)
			   (vector-append prefix first-prog postfix)
			   constraint extra #:assume assumption))
	  
	  (cond
	   [ce
	    (when debug (pretty-display "[1.1] counterexample"))
	    (define key (cons states-vec my-liveout-vec))
	    (define input (send simulator interpret prefix ce #:dep #f))
	    (define expected-output (send simulator interpret spec input #:dep #f))
	    (define expected-output-vec (send machine progstate->vector expected-output))
	    (define my-classes (make-hash))
	    (define iterator (get-collection-iterator prog))
	    (define (loop p)
	      (when p
		    (define output 
		      (with-handlers*
		       ([exn? (lambda (e) #f)])
		       (send simulator interpret p input #:dep #f)))
		    (when 
		     output
		     (hash-set! my-classes 
				(cons (list (send machine progstate->vector output)) my-liveout-vec)
				(list p)))
		    (loop (iterator))))
	    (loop (iterator))
	    (hash-set! classes key (entry input expected-output-vec my-classes))
	    ]

	   [else
	    (pretty-display "[1.2] FOUND!!!!")
	    (define groups (hash-keys classes))
	    (pretty-display `(groups ,(length groups)))
	    (send printer print-syntax (send printer decode first-prog))
	    (yield first-prog)])
          ]

	 [else
	  (when debug (pretty-display "[2] incorrect"))
	  (let ([key (cons states-vec my-liveout-vec)])
	    (hash-set! classes key (list prog)))]))

      (define (insert-table table key collection states-vec-spec states-vec)
	(define live-out (cdr key))

	(define (insert-extra input expected-output-vec small-table)
	  (when debug 
		(pretty-display `(insert-extra))
		(send machine display-state input))
	  (define iterator (get-collection-iterator collection))
	  (define (loop prog)
	    (when prog
		  (define output 
		    (with-handlers*
		     ([exn? (lambda (e) #f)])
		     (send simulator interpret prog input #:dep #f)))
		  (when output
			(define output-vec (send machine progstate->vector output))
			(define small-key (cons (list output-vec) live-out))
			(insert-table small-table small-key prog 
				      (list expected-output-vec) (list output-vec)))
		  (loop (iterator))))
	  (loop (iterator)))
	
	(if (hash-has-key? table key)
	    (let ([val (hash-ref table key)])
	      (if (list? val)
		  (begin
		    (when debug (pretty-display "[a] append to list"))
		    (hash-set! table key (cons collection (hash-ref table key)))
		    )
		  (begin
		    (when debug (pretty-display "[b] expand table"))
		    (insert-extra (entry-input val) (entry-outputvec val) (entry-table val)))))
	    (begin
	      (when debug (pretty-display "[c] new entry"))
	      (check-and-insert-new table collection states-vec-spec states-vec))))

      (when debug (pretty-display `(insert-table ,prog)))
      (insert-table classes (cons states-vec my-liveout-vec) prog states-vec-spec states-vec)

      )

		

    ))
