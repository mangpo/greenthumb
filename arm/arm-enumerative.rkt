#lang racket

(require "../ast.rkt" "../enumerative.rkt" "../machine.rkt" "../ops-racket.rkt"
	 "arm-ast.rkt" "arm-machine.rkt" "arm-simulator-racket.rkt" "arm-validator.rkt")
(require racket/generator)

(provide arm-enumerative%)

(define arm-enumerative%
  (class enumerative%
    (super-new)
    (inherit-field machine printer simulator validator generate-inst
                   t-get-type t-later-use)
    (inherit lexical-cmp)
    (override len-limit window-size reset-generate-inst 
	      get-register-mapping get-renaming-iterator
	      abstract lexical-skeleton get-flag get-output-location)

    (define (len-limit) 2)
    (define (window-size) 4)
    (set! simulator (new arm-simulator-racket% [machine machine]))
    (set! validator (new arm-validator% [machine machine] [printer printer]))

    (define inst-id (get-field inst-id machine))
    (define inst-with-shf (get-field inst-with-shf machine))
    (define cond-type-len (vector-length (get-field cond-inst-id machine)))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define shf-inst-len (vector-length shf-inst-id))

    (define inst-mod '(add sub rsb
			and orr eor bic orn
                        mul uxtah
			add# sub# rsb#
			and# orr# eor# bic# orn#
			lsl#
			mov mvn
			uxth uxtb
			mov# mvn# movw# movt#
			mla mls
			bfc bfi))

    (define inst-high '(lsr# asr#))

    (define commutative-0-1 '(tst))
    (define commutative-1-2 '(add and orr eor mul smmul))
    (define commutative-2-3 '(mla smull umull smmla))

    ;; If regs is not #f, use virtual registers
    ;; If lex is not #f, impose lexical order. This is only valid with virtual registers.
    (define (reset-generate-inst states live-in regs type lex 
				 ;; #:live-limit [limit #f] 
                                 #:no-args [no-args #f])
      ;; (when (and limit live-in (> (length live-in) limit))
      ;; 	    (set! live-in (take live-in limit)))
      (define mode (cond [regs `vir] [no-args `no-args] [else `basic]))
      (define z (progstate-z (car states))) ;; enough to look at one state.
      ;; (define inst-choice '(sub# clz mvn# rsb))
      ;; (define inst-pool (map (lambda (x) (vector-member x inst-id)) inst-choice))
      (define inst-pool (get-field inst-pool machine))
      (cond
       [(equal? type `mod) 
	(set! inst-pool (filter (lambda (x) (member (vector-ref inst-id x) inst-mod)) inst-pool))]
       [(equal? type `high)
       	(set! inst-pool (filter (lambda (x) (member (vector-ref inst-id x) inst-high)) inst-pool))]
       )
	
      (set! generate-inst 
	    (generator 
	     ()
	     (when debug (pretty-display `(reset-generate-inst ,inst-pool)))

	     (define (recurse-args opcode opcode-id shfop shfarg cond-type args ranges v-reg)
	       (when debug (pretty-display `(recurse-args ,args ,ranges ,shfop ,v-reg)))
	       ;; Symmetry reduction for commutative operations
	       (define pass
		 (cond
		  [(and (= (length args) 2) (member opcode commutative-0-1) (= shfop 0))
		   (<= (second args) (first args))]
		  [(and (= (length args) 3) (member opcode commutative-1-2) (= shfop 0))
		   (<= (second args) (first args))]
		  [(and (= (length args) 4) (member opcode commutative-2-3) (= shfop 0))
		   (<= (second args) (first args))]
		  [else #t]))
	       (when
		pass
		(cond
		 [(empty? ranges)
		  (let* ([i (arm-inst opcode-id (list->vector (reverse args)) shfop shfarg cond-type)]
			 [ret (list i (send machine update-live live-in i) v-reg)])
		    (if lex
		    	(let ([my-lex (lexical-skeleton i)])
		    	  (if my-lex
		    	      (when (>= (lexical-cmp my-lex lex) 0)
		    		    (yield ret))
		    	      (yield ret)))
		    	(yield ret)))
		  ]

		 [(equal? (car ranges) `reg-o)
		  (if (pair? v-reg)
		      ;; enumerate no-arg
		      (recurse-args opcode opcode-id shfop shfarg cond-type 
				    (cons (cdr v-reg) args)
				    (cdr ranges) (cons (car v-reg) (add1 (cdr v-reg))))
		      ;; enumerate virtual
		      (recurse-args opcode opcode-id shfop shfarg cond-type 
				    (cons v-reg args)
				    (cdr ranges) (add1 v-reg)))
		      ]

		 [(equal? (car ranges) `reg-i)
		  ;; enumerate no-arg
		  (recurse-args opcode opcode-id shfop shfarg cond-type 
				(cons (car v-reg) args)
				(cdr ranges) (cons (add1 (car v-reg)) (cdr v-reg)))
		  ]

		 [else
		  (for ([arg (car ranges)])
		       (recurse-args opcode opcode-id shfop shfarg cond-type 
				     (cons arg args)
				     (cdr ranges) v-reg))
		  ])))

	     (for ([opcode-id inst-pool])
		  (let ([opcode-name (vector-ref inst-id opcode-id)]
			[cond1 (or (not (equal? type `rest))
				   (and (equal? type `rest)
					(not (member (vector-ref inst-id opcode-id) inst-mod))
					(not (member (vector-ref inst-id opcode-id) inst-high))
					     ))]
			[cond2 (or (equal? type `rest) (equal? type `all))]
			)
		    (unless 
		     (equal? opcode-name `nop)
		     (let* ([shf? (member opcode-name inst-with-shf)]
			    [arg-ranges 
			     (vector->list 
			      (send machine get-arg-ranges opcode-name #f live-in 
				    #:mode mode))]
			    [v-reg (cond [regs regs] [no-args (cons 0 3)] [else #f])]
			    [cond-bound (if (or (= z -1) regs) 1 cond-type-len)]) ;; TODO
		       (when debug (pretty-display `(iterate ,opcode-name ,arg-ranges ,cond-bound)))
		       (for ([cond-type cond-bound])
			    (if shf?
				(begin
				  ;; no shift
				  (when
				   cond1
				   (recurse-args opcode-name opcode-id 0 #f cond-type 
						 (list) arg-ranges v-reg)
				   ;; lsl#
				   (let ([shfop 1])
				     (for ([shfarg (send machine get-shfarg-range shfop live-in)])
					  (recurse-args opcode-name opcode-id shfop shfarg cond-type 
							(list) arg-ranges v-reg)))
				   )
				  ;; shift (except lsl#)
				  (when 
				   cond2
				   (for ([shfop (range 2 shf-inst-len)])
					(let ([shfarg-range 
					       (send machine get-shfarg-range shfop live-in 
						     #:mode mode)])
					  (if (equal? shfarg-range `reg-i)
					      (recurse-args opcode-name opcode-id shfop 0 cond-type 
							    (list) arg-ranges (cons 1 3))
					      (for ([shfarg shfarg-range])
						   (recurse-args opcode-name opcode-id 
								 shfop shfarg cond-type 
								 (list) arg-ranges v-reg)))))
				   )
				  )
				;; no shift
				(when
				 cond1
				 (recurse-args opcode-name opcode-id 0 #f cond-type 
					       (list) arg-ranges v-reg))
				))))))
	     (yield (list #f #f #f)))))

    (define (get-register-mapping org-nregs states-vec-spec states-vec liveout-vec)
      ;;(pretty-display `(get-register-mapping ,states-vec-spec ,states-vec ,liveout-vec))
      (define all-nregs (send machine get-nregs))
      (define regs-spec (map (lambda (x) (vector-ref x 0)) states-vec-spec)) ;; list
      (define regs (map (lambda (x) (vector-ref x 0)) states-vec))
      (define liveout (vector-ref liveout-vec 0))
      (define mapping (make-vector org-nregs #f))
      (define break #f)

      (for ([i org-nregs] #:break break)
      	   (when 
      	    (vector-ref liveout i)
      	    (vector-set! mapping i (list))
      	    (let ([regs-spec-i (map (lambda (x) (vector-ref x i)) regs-spec)])
      	      (for ([j all-nregs])
      		   (let* ([regs-j (map (lambda (x) (vector-ref x j)) regs)]
      			  [compare
      			   (for/and ([spec regs-spec-i]
      			   	     [my regs-j])
      			   	    (equal? spec my))])
      		     (when compare
      		     	   (vector-set! mapping i (cons j (vector-ref mapping i)))))))
      	    (when (empty? (vector-ref mapping i))
      		  (set! break #t))))
      
      ;;(pretty-display `(get-register-mapping ,(and (not break) mapping)))
      (and (not break) mapping))
    
    (define (check-later-use prog org index len)
      (define (inner x)
	(define opcode-name (send machine get-inst-name (inst-op x)))
	(for/and ([arg (inst-args x)]
		  [type (send machine get-arg-types opcode-name)])
		 (or (not (member type '(reg-o reg-i reg-io)))
		     (not (= arg org)))))
      
      (for/and ([i (range (add1 index) len)])
        (inner (vector-ref prog i))))


    (define (rename prog assigned)
      (define t00 (current-milliseconds))
      ;(pretty-display `(rename ,assigned))
      (define len (vector-length prog))
      (define valid #t)
      (define table (make-vector (send machine get-nregs) #f))
      (for ([x assigned]
	    [i (length assigned)])
	   (when x (vector-set! table x i)))

      (define (rename-inst x index)
	(define opcode (inst-op x))
	(define opcode-name (send machine get-inst-name opcode))
	(define args (inst-args x))
	(define cond-type (inst-cond x))
	(define shfop (inst-shfop x))
	(define shfarg (inst-shfarg x))
        (define t0 (current-milliseconds))
        (define types (send machine get-arg-types opcode-name))
        (define t1 (current-milliseconds))
        (set! t-get-type (+ t-get-type (- t1 t0)))
	(define new-args
	  (for/vector 
	   ([arg args]
	    [type types])
	   (cond
	    [(member type '(reg-o reg-i reg-io))
	     (let ([to (vector-ref table arg)])
	       (if to 
		   (let ([res (or (= arg to) (check-later-use prog to index len))])
		     (set! valid (and valid res))
		     to)
		   arg))]
	    [else arg])))
	
	(arm-inst opcode new-args shfop shfarg cond-type)
	)

      (let ([new-x
             (for/vector ([x prog]
                          [i (vector-length prog)]
                          #:break (not valid))
			 ;;(pretty-display `(rename-inst ,i))
               (rename-inst x i))])
        ;; (unless valid
        ;;   (pretty-display "Rename: invalid")
        ;;   (send printer print-syntax (send printer decode prog))
        ;;   )
        (define t11 (current-milliseconds))
        (set! t-later-use (+ t-later-use (- t11 t00)))
        (and valid new-x))
      )

    (define (get-renaming-iterator prog mapping out-loc)
      ;(pretty-display `(get-renaming-iterator ,mapping))
      (define ans (list))
      (define (recurse index assigned mapping)
        (cond
         [(empty? mapping)
          (when (member out-loc assigned)
                (define renamed (rename prog assigned))
                (when renamed (set! ans (cons renamed ans))))]
         [(car mapping)
          (for ([choice (car mapping)])
               (recurse (add1 index) (cons choice assigned) (cdr mapping)))]

         [else (recurse (add1 index) (cons #f assigned) (cdr mapping))]
         ))
      (recurse 0 (list) (reverse (vector->list mapping)))
      ans)

    ;; TODO: memory, z
    (define (abstract state-vec live-list f)
      (define regs (vector-ref state-vec 0))
      (define mems (vector-ref state-vec 1))
      (define z (vector-ref state-vec 2))
      (define fp (vector-ref state-vec 3))
      (vector
       (for/vector ([r regs] [i (vector-length regs)])
		   (and (member i live-list) (f r)))
       (vector-copy mems)
       z fp))

    (define (lexical-skeleton x)
      (define opcode-id (inst-op x))
      (define opcode-name (vector-ref inst-id opcode-id))
      (cond
       [(member opcode-name '(tst cmp tst# cmp#)) 
	#f]

       [else
	(define args (inst-args x))
	(define shfop (inst-shfop x))
	(define shfarg (inst-shfarg x))
	(define args-ret (list))
	(define ops-ret #f)
	(for ([c (send machine get-arg-types opcode-name)] ;; TODO: memory
	      [arg (inst-args x)])
	     (when (or (equal? c `reg-i) (equal? c `reg-io))
		   (set! args-ret (cons arg args-ret))))
	(when (member (vector-ref shf-inst-id shfop) '(asr lsl lsr))
	      (set! args-ret (cons shfarg args-ret)))
	(set! ops-ret (list opcode-id shfop))
	(list (sort args-ret >) (reverse args-ret) ops-ret)]))

    (define (get-flag state-vec) 
      (define z (vector-ref state-vec 2))
      (= z -1))


    (define (get-output-location my-inst)
      (for/or ([type (send machine get-arg-types (vector-ref inst-id (inst-op my-inst)))]
	       [arg (inst-args my-inst)])
	      (and (member type '(reg-o reg-io)) arg)))
    ))
