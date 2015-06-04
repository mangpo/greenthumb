#lang racket

(require "ast.rkt" "machine.rkt" "decomposer.rkt" 
         "arm/arm-psql.rkt" "arm/arm-simulator-abstract.rkt" "arm/arm-ast.rkt")
(require racket/generator profile)

(provide enumerative%)

(struct concat (collection inst))

(define-syntax-rule (entry live vreg flag) (list live vreg flag))
(define-syntax-rule (entry-live x) (first x))
(define-syntax-rule (entry-vreg x) (second x))
(define-syntax-rule (entry-flag x) (third x))

(define enumerative%
  (class decomposer%
    (super-new)
    (init-field [generate-inst #f] [t-get-type 0] [t-later-use 0])
    (inherit-field machine printer validator simulator)
    (override synthesize-window)
    (abstract reset-generate-inst abstract lexical-skeleton)
    (public get-register-mapping get-renaming-iterator build-db
	    lexical-cmp get-flag get-output-location)

    (define bit (get-field bit machine))
    (define live-limit 3)

    (define (synthesize-window spec sketch prefix postfix constraint extra 
                               [cost #f] [time-limit 3600]
                               #:hard-prefix [hard-prefix (vector)] 
                               #:hard-postfix [hard-postfix (vector)]
                               #:assume-interpret [assume-interpret #t]
                               #:assume [assumption (send machine no-assumption)])
      (define ttt (current-seconds))
      (define abst (new arm-simulator-abstract% [k 3] [machine machine]))
      (send abst load-abstract-behavior)

      (define spec-len (vector-length spec))
      (define org-nregs (send machine get-nregs)) ;; #f is default
      (send machine analyze-opcode prefix spec postfix)
      (define virtual (send machine is-virtual-reg))
      (send machine analyze-args prefix spec postfix #:vreg (if virtual spec-len 0))

      (define live2 (send validator get-live-in postfix constraint extra))
      (define live2-vec (send machine progstate->vector live2))
      (define live1 (send validator get-live-in spec live2 extra))
      (define live1-list (send machine get-operand-live live1))
      (define live2-list (send machine get-operand-live live2))

      (define ntests 2)
      (define inits
	(send validator generate-input-states ntests (vector-append prefix spec postfix)
              assumption extra))

      (define-syntax-rule (take-high lst) (take lst 1))
      (define-syntax-rule (take-mod lst) (drop lst 1))

      (define prev-classes (make-hash))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states1-vec 
	(map (lambda (x) (send machine progstate->vector x)) states1))
      (define states2-vec-spec
	(map (lambda (x) 
	       (send machine progstate->vector 
		     (send simulator interpret spec x #:dep #f))) states1))

      (define expect-mod (make-vector 3))
      (define expect-high (make-vector 3))

      (for ([k (list 1 2 3)])
           (let* ([base (arithmetic-shift 1 k)]
                  [f (lambda (x) (modulo x base))])
             (vector-set! expect-mod (- k 1) 
                          (map (lambda (x) (abstract x live2-list f)) 
                               (take-mod states2-vec-spec)))))

      (for ([k (list 1 2 3)])
           (let* ([mask (arithmetic-shift -1 (- bit k))]
                  [f (lambda (x) (bitwise-and x mask))])
             (vector-set! expect-high (- k 1) 
                          (map (lambda (x) (abstract x live2-list f))
                               (take-high states2-vec-spec)))))
             
      (pretty-display `(states1-vec ,states1-vec))
      (pretty-display `(states2-vec-spec ,states2-vec-spec))
      (pretty-display `(live2-vec ,live2-vec))
      (pretty-display (format "VIRTUAL = ~a" virtual))
      ;; key = (cons list of output-vec, liveout-vec)
      ;; (hash-set! prev-classes 
      ;; 		 (entry (send machine get-operand-live live1) 
      ;; 			org-nregs 
      ;; 			(get-flag (car states1)))
      ;; 		 (make-hash (list (cons states1-vec (list (vector))))))
      (class-insert! prev-classes 
		     live1-list org-nregs  
		     states1-vec (vector))

      (for ([i states1])
	   (send machine display-state i))

      (define t-start (current-seconds))
      (define t-refine 0)
      (define t-abst 0)
      (define count-t 0)
      (define count-1 0)

      (define t-collect 0)
      (define t-abst-inter 0)
      (define t-abst-inter2 0)
      (define t-real-inter 0)
      (define t-mapping 0)
      (define t-rename 0)
      (define t-check 0)
      (define t-extra 0)
      (define count-p 0)
      (define count-r 0)
      (define count-abst 0)
      (define count-real 0)

      (define candidate-gen
	(generator
	 ()
	 (define (loop iter)
	   (pretty-display `(time ,(- (current-seconds) ttt)))
	   (newline)
	   (pretty-display `(loop ,iter))
	   (define classes (make-hash))
	   (define ce-list (list))

	   (define (check-eqv prog states-vec out-loc)
	     ;(pretty-display `(check-eqv ,prog ,states-vec))

	     ;; (when (concat? prog) 
	     ;; 	   (let ([x (concat-inst prog)])
             ;;         (when (and (equal? `rsb 
             ;;                            (vector-ref (get-field inst-id machine) (inst-op x)))
             ;;                    (equal? `lsr 
             ;;                            (vector-ref (get-field shf-inst-id machine) 
             ;;                                        (inst-shfop x)))
             ;;                    (equal? 2 (inst-shfarg x))
             ;;                    (equal? 4 (vector-ref (inst-args x) 0))
             ;;                    (equal? 3 (vector-ref (inst-args x) 1))
             ;;                    (equal? 3 (vector-ref (inst-args x) 2)))
             ;;               (newline)
	     ;; 		   (pretty-display `(check-eqv ,live2-vec
	     ;; 					       ,states-vec
	     ;; 					       ,states2-vec-spec))
             ;;               (newline))))

	     ;; STEP 1: check equivalence on test cases
             (set! count-1 (add1 count-1))
             (define t-refine-start (current-milliseconds))
             (when debug (pretty-display "[1] correct on first query"))

             ;; STEP 2: try on extra test cases
             (define (inner-loop iterator)
               (define t0 (current-milliseconds))
               (define p (and (not (empty? iterator)) (car iterator)))
               (define t1 (current-milliseconds))
               (set! t-rename (+ t-rename (- t1 t0)))
               (when p
                     (set! count-r (add1 count-r))
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
                            (when debug (pretty-display "[3] counterexample"))
                            (set! ce-list (cons (cons ce-input ce-output-vec) ce-list))
                            )
                          (begin
                            (pretty-display "[4] FOUND!!!")
                            (let ([total (- (current-seconds) t-start)])
                              (when (> total 0)
                                    (pretty-display 
                                     `(time ,count-1 ,count-t
                                            ,(exact->inexact (/ count-1 count-t))
                                            ,(exact->inexact (/ t-refine 1000 
                                                                (- (current-seconds) t-start)))
                                            ,(exact->inexact (/ t-abst 1000 
                                                                (- (current-seconds) t-start)))))))
                            (let ([groups (hash-keys classes)])
                              (send printer print-syntax (send printer decode p))
                              (pretty-display `(groups ,(length groups))))
                            (yield p)))
                      ) ;; when all extras are correct.
                     (let ([t2 (current-milliseconds)])
                       (set! t-extra (+ t-extra (- t2 t1))))
                     (inner-loop (cdr iterator))))

             ;; mapping and loop is for renaming virtual registers.
             (define mapping 
               (get-register-mapping org-nregs states2-vec-spec states-vec live2-vec))

             (define (loop iterator)
               (define p (iterator))
               (when p 
                     (set! count-p (add1 count-p))
                     ;; (newline)
                     ;; ;; (pretty-display "Before renaming")
                     ;; (send printer print-syntax (send printer decode p))
                     (when mapping
                           (define t0 (current-milliseconds))
                           (define iterator2 (get-renaming-iterator p mapping out-loc))
                           (define t1 (current-milliseconds))
                           (set! t-rename (+ t-rename (- t1 t0)))
                           (inner-loop iterator2)
                           )
                     (loop iterator))
               )

             (if virtual
                 (when mapping (loop (get-collection-iterator prog)))
                 (inner-loop (get-collection-iterator prog)))
             (set! t-refine (+ t-refine (- (current-milliseconds) t-refine-start)))
	     ) ;; End check-eqv

	   (define (refine-real classes live-list my-vreg my-inst out-loc)
             (define n (length classes))
             (set! count-real (+ count-real n))

             (define (check-loop states states2-vec-spec keep states-org)
               (cond
                [(empty? states) 
                 (check-eqv (concat (class-ref prev-classes live-list my-vreg states-org) 
                                    my-inst)
                            (reverse keep) out-loc)]

                [else
                 (define t0 (current-milliseconds))
                 (define out
                   (with-handlers*
                    ([exn? (lambda (e) #f)])
                    (send machine progstate->vector 
                          (send simulator interpret (vector my-inst)
                                (send machine vector->progstate (car states))
                                #:dep #f))))

                 (define t1 (current-milliseconds))
                 (define pass
                   (and 
                    out
                    (if virtual
                        (send machine relaxed-state-eq? 
                              (car states2-vec-spec) out live2-vec out-loc)
                        (send machine state-eq? 
                              (car states2-vec-spec) out live2-vec))))
                 (define t2 (current-milliseconds))

                 (set! t-real-inter (+ t-real-inter (- t1 t0)))
                 (set! t-check (+ t-check (- t2 t1)))

                 (when pass (check-loop (cdr states) (cdr states2-vec-spec)
                                        (cons out keep) states-org))]
                ))

             (for ([states classes])
                  (set! count-t (add1 count-t))
                  (check-loop states states2-vec-spec (list) states)))

	   (define (refine-abstract classes live-list my-vreg new-live-list my-inst k type out-loc)
	     (define f-mod
	       (let ([base (arithmetic-shift 1 k)])
		 (lambda (x) (modulo x base))))

	     (define f-high
	       (let ([mask (arithmetic-shift -1 (- bit k))])
		 (lambda (x) (bitwise-and x mask))))
	       
	     (define abst-hash classes)
	     (define abst-expect-mod (vector-ref expect-mod (- k 1)))
	     (define abst-expect-high (vector-ref expect-high (- k 1)))

	     (when (list? abst-hash)
		   ;; TODO: live-list should contain memory as well
		   (set! abst-hash (make-hash))
		   (for ([states classes])
			(let* ([abst-states-mod 
				(map (lambda (x) (abstract x live-list f-mod)) 
                                     (take-mod states))]
			       [abst-states-high 
				(map (lambda (x) (abstract x live-list f-high))
                                     (take-high states))]
			       [abst-states (cons abst-states-mod abst-states-high)]
			       )
                          ;;(pretty-display `(? ,bit ,states ,abst-states-mod ,abst-states-high))
			  (if (hash-has-key? abst-hash abst-states)
			      (hash-set! abst-hash abst-states
					 (cons states (hash-ref abst-hash abst-states)))
			      (begin
				;; (pretty-display `(insert-mod ,abst-states-mod))
				;; (pretty-display `(insert-high ,abst-states-high))
				(hash-set! abst-hash abst-states (list states))
				)
			      )))
		   
		   ;; (pretty-display `(live-list ,live-list))
		   ;; (pretty-display `(abst-hash ,(hash-count abst-hash)))
		   )

	     (define (interpret-real f) 
	       (lambda (x)
		 (abstract 
		  (send machine progstate->vector 
			(send simulator interpret (vector my-inst)
			      (send machine vector->progstate x) #:dep #f))
		  new-live-list f)))
	     (define (interpret-abst f abst-type)
	       (lambda (x)
		 (let ([out-list (send abst interpret-inst my-inst
				       (send machine vector->progstate x) abst-type k)])
		   (if (list? out-list)
		       (for/list ([out out-list])
				 (abstract (send machine progstate->vector out)
					   new-live-list f))
		       out-list))))

             (define interpret-mod 
               (if (member type '(mod+high mod-high))
		   (interpret-real f-mod)
		   (interpret-abst f-mod `mod)))
             (define interpret-high 
               (if (member type '(mod+high high-mod))
		   (interpret-real f-high)
		   (interpret-abst f-high `high)))

	     (define (check-abst state-spec state)
		(or 
		 (equal? state #t)
                 (universal? state)
		 (for/or ([s (if (list? state) state (list state))])
			 (if virtual
			     (send machine relaxed-state-eq? state-spec s live2-vec out-loc)
			     (send machine state-eq? state-spec s live2-vec)))))
             
             (define n (hash-count abst-hash))
             (set! count-abst (+ count-abst n))
             ;(pretty-display `(refine-abst ,k ,n))

	     (define (recurse mod-final high-final mod-list high-list mod-expect high-expect 
			      real-states)
	       ;;(pretty-display `(recurse ,mod-list ,high-list))
	       (cond
		[(and (empty? mod-list) (empty? high-list)
		      (= 0 (count (lambda (x) (not (boolean? x))) mod-final))
		      (= 0 (count (lambda (x) (not (boolean? x))) high-final)))
		 (define t0 (current-milliseconds))
		 (define ret (collect-states real-states))
		 (define t1 (current-milliseconds))
		 (set! t-collect (+ t-collect (- t1 t0)))
		 (refine-real ret live-list my-vreg my-inst out-loc)
		 ]

		[(and (empty? mod-list) (empty? high-list))
		 (if (< k 3)
		     (hash-set! 
		      abst-hash
		      (cons (reverse mod-final) (reverse high-final))
		      (refine-abstract real-states live-list my-vreg new-live-list 
				       my-inst 
				       (add1 k) type out-loc))
		     (begin
		       ;; (pretty-display 
		       ;;  `(abst ,abst-states ,abst-states-out ,abst-expect ,live2-vec))
		       ;; 
		       (refine-real real-states live-list my-vreg my-inst out-loc)
		       )
		     )
		 ]
		
		[(empty? mod-list)
		 (define t0 (current-milliseconds))
		 (define out (interpret-high (car high-list)))
		 (define t1 (current-milliseconds))
		 (set! t-abst-inter2 (+ t-abst-inter2 (- t1 t0)))
		 (when (and out (check-abst (car high-expect) out))
		       (recurse mod-final (cons (car high-list) high-final) 
				mod-list (cdr high-list)
				mod-expect (cdr high-expect) real-states))
		 ]

		[else
		 (define t0 (current-milliseconds))
		 (define out (interpret-mod (car mod-list)))
		 (define t1 (current-milliseconds))
		 (set! t-abst-inter (+ t-abst-inter (- t1 t0)))
		 (when (and out (check-abst (car mod-expect) out))
		       (recurse (cons (car mod-list) mod-final) high-final 
				(cdr mod-list) high-list
				(cdr mod-expect) high-expect real-states))])
	       ) 

	     (for ([pair (hash->list abst-hash)]
                   [i n])
		  (let* ([abst-states (car pair)]
			 [abst-states-mod (car abst-states)]
			 [abst-states-high (cdr abst-states)]
			 [real-states (cdr pair)])
		    ;; (pretty-display `(mod ,abst-states-mod))
		    ;; (pretty-display `(high ,abst-states-high))
		    (recurse (list) (list) abst-states-mod abst-states-high 
			     abst-expect-mod abst-expect-high real-states)))

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
			(class-insert! classes my-liveout my-vreg states2-vec prog)
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
             (send abst reset-cache)

             (when my-inst (send printer print-syntax (send printer decode my-inst)))
             (define t0 (current-milliseconds))
             (set! t-collect 0) (set! t-abst-inter 0) (set! t-abst-inter2 0) (set! t-real-inter 0) (set! t-real-inter 0) (set! t-rename 0) (set! t-check 0) (set! t-get-type 0) (set! t-later-use 0) (set! t-extra 0) 
             (set! count-p 0) (set! count-r 0) (set! count-abst 0) (set! count-real 0)
	     (if my-inst
                 (let* ([out-loc (get-output-location my-inst)]
                        [abst-hash 
                         (refine-abstract eqv-classes live-list my-vreg my-liveout my-inst 
                                          1 type out-loc)])
                   (pretty-display (format "~a ms = ~a (~a+~a/~a) ~a/~a ~a ~a ~a ~a ~a | ~a ~a" 
                                           (- (current-milliseconds) t0)
                                           t-collect 
					   t-abst-inter t-abst-inter2 count-abst 
                                           t-real-inter count-real
                                           t-check t-rename t-get-type t-later-use t-extra
                                           count-p count-r))
	   	   (abst-loop abst-hash live-list my-vreg type))
                 eqv-classes))

           (when (> iter 2)
	   (for ([pair1 (hash->list prev-classes)])
	   	(let* ([live-vreg (car pair1)]
	   	       [live-list (entry-live live-vreg)]
	   	       [my-vreg (entry-vreg live-vreg)]
	   	       [hash2 (cdr pair1)]
	   	       [eqv-classes (hash-keys hash2)]
		       ;; use only first state
		       [state-rep-list (list (send machine vector->progstate 
                                                   (caar eqv-classes)))]
                       [abst-hash eqv-classes])
		  (pretty-display `(key ,live-vreg ,(length eqv-classes)))
		  (for ([type '(rest mod+high mod-high high-mod)])
		       (newline)
		       (pretty-display (format "TYPE: ~a" type))
		       (reset-generate-inst state-rep-list live-list (and virtual my-vreg) 
					    type #f) 
		       (set! abst-hash (abst-loop abst-hash live-list my-vreg type)))
		  ))
           )

	   (set! t-abst (+ t-abst (- (current-milliseconds) t-abst-start)))
	   (pretty-display `(abstract-done))

	   ;; Grow
	   (for ([pair1 (hash->list prev-classes)])
		(let* ([live-vreg (car pair1)]
		       [live-list (entry-live live-vreg)]
		       [my-vreg (entry-vreg live-vreg)]
		       [hash2 (cdr pair1)])
		  (pretty-display `(key ,live-vreg ,(hash-count hash2)))
		  (for ([pair2 (hash->list hash2)])
		       (let* ([val (cdr pair2)]
			      [outputs (map (lambda (x) (send machine vector->progstate x)) 
					    (car pair2))]
			      [smallest-lex (and virtual (get-smallest-lex val))]
			      )
			 ;; Initialize enumeration one instruction process
			 (when debug
			       (pretty-display `(ENUM!!!!!!!!!!!!! ,val))
			       (print-concat val)
			       )
			 (reset-generate-inst outputs live-list (and virtual my-vreg)
					      `all smallest-lex); #:live-limit 3)
			 (enumerate outputs val #f) ;; no check
			 ))))
	   (when (< iter spec-len)
		 (pretty-display `(iter ,iter ,spec-len))
		 (set! prev-classes classes)
		 (loop (add1 iter))))
	 (loop 0)))

      (print-concat (candidate-gen))
      )

    (define (collect-states x)
      (if (list? x)
	  x
	  (let ([ans (list)])
            (for ([val (hash-values x)])
                 (set! ans (append (collect-states val) ans)))
            ans)))

    (define (get-flag state) #f)

    (define (class-insert! class live-list my-vreg states-vec prog)
      (when (> (length live-list) live-limit)
      	    (set! live-list (take live-list live-limit)))
      (set! states-vec (map (lambda (x) (abstract x live-list identity)) states-vec))
      (define key (entry live-list my-vreg (get-flag (car states-vec))))
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
      (hash-ref (hash-ref class (entry live-vec my-vreg (get-flag (car states-vec)))) states-vec))

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
      (define all-states-str
        (map (lambda (x) (send psql progstate->string x)) all-states))
      (define all-states-vec 
        (map (lambda (x) (send machine progstate->vector x)) all-states))
      
      (send psql db-connect)
      
      (send time reset)
      (define max-size 2)
      (define prev-classes (make-hash))
      (hash-set! prev-classes all-states-vec (list (vector)))
      (send psql create-table 0)
      ;; (send psql insert 0 all-states-str all-states (vector))
      (send psql bulk-insert 0 prev-classes all-states-str #t)

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
                            (when (< len max-size) (update-hash prev-classes classes))
                            (send psql bulk-insert len classes all-states-str #t)
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

        (define (update-hash hash1 hash2)
          (send time start `hash)
          (for ([pair (hash->list hash2)])
               (let ([key (car pair)]
                     [val (cdr pair)])
                 (if (hash-has-key? hash1 key)
                     (hash-set! hash1 key (append (hash-ref hash1 key) val))
                     (hash-set! hash1 key val))))
          (send time end `hash)
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
             
             (when #t
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
            
        (send psql create-table len)

        ;; (define data (send psql select-all (sub1 len)))
        ;; (send time reset)
        ;; (for ([x data])
        ;;      (let ([progs (record-progs x)]
        ;;            [outputs (record-states x)])
        ;;        (reset-generate-inst outputs live-list #f `all #f)
        ;;        (enumerate outputs progs)))

        (send time start `hash)
        (define pairs (hash->list prev-classes))
        (set! prev-classes (make-hash)) ;; prepare to save current groups
        (send time end `hash)
        (for ([pair pairs])
             (let ([outputs 
                    (map (lambda (x) (send machine vector->progstate x)) (car pair))]
                   [progs (cdr pair)])
               (reset-generate-inst outputs live-list #f `all #f)
               (enumerate outputs progs)))

        (when (< len max-size) (update-hash prev-classes classes))
        (send psql bulk-insert len classes all-states-str #t)
        (set! classes (make-hash))
        (set! my-count 0)
        (set! all-count 0)
        (collect-garbage)
        (send psql create-index len)

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

    (define (get-output-location my-inst) #f)

    ))
