#lang racket

(require "arm/arm-machine.rkt" "ast.rkt" "arm/arm-ast.rkt")
(provide forwardbackward%)

(struct concat (collection inst))
(struct box (val))

(define-syntax-rule (entry live flag) (list live flag))
(define-syntax-rule (entry-live x) (first x))
(define-syntax-rule (entry-flag x) (second x))

(define forwardbackward%
  (class object%
    (super-new)
    (init-field machine enum simulator simulator-precise
                printer parser
                validator validator-precise
		inverse)
    (abstract vector->id mask-in inst->vector
              reduce-precision increase-precision
	      get-live-mask)
    (public synthesize-window)
    
    (define debug #f)
    (define ce-limit 100)
    (define try-cmp #f)

    (define c-behaviors 0)
    (define c-progs 0)
    (define (class-insert! class live states-vec prog)
      (set! c-progs (add1 c-progs))

      (define (insert-inner x states-vec prog)
        (define key (car states-vec))
        (if (= (length states-vec) 1)
	    (if (hash-has-key? x key)
		(hash-set! x key (cons prog (hash-ref x key)))
		(begin
		  (set! c-behaviors (add1 c-behaviors))
		  (hash-set! x key (list prog))))
	    (let ([has-key (hash-has-key? x key)])
	      (unless has-key (hash-set! x key (make-hash)))
	      (insert-inner (hash-ref x key) (cdr states-vec) prog))))

      ;(set! states-vec (map (lambda (x) (abstract x live-list identity)) states-vec))
      (define key (entry (cons (sort (car live) <) (sort (cdr live) <))
			 ;;(cons (car live) (sort (drop live 1) <))
			 (send enum get-flag (car states-vec))))
      (unless (hash-has-key? class key) (hash-set! class key (make-hash)))
      (insert-inner (hash-ref class key) states-vec prog))

    (define c-behaviors-bw 0)
    (define c-progs-bw 0)

    (define instvec2id (make-hash))
    (define id2inst (make-vector 100000 #f))
    (define last-id 0)

    (define prog2id (make-hash))
    (define id2prog (make-vector 10000000 #f))
    (define last-prog-id 1)

    (define (prog->id prog)
      (unless (hash-has-key? prog2id prog)
	      (hash-set! prog2id prog last-prog-id)
	      (vector-set! id2prog last-prog-id prog)
	      (set! c-progs-bw (add1 c-progs-bw))
	      (set! last-prog-id (add1 last-prog-id)))
      (hash-ref prog2id prog))

    (define (id->prog id) (vector-ref id2prog id))

    (define (inst->id my-inst)
      (define inst-vec (inst->vector my-inst))
      (unless (hash-has-key? instvec2id inst-vec)
	      (hash-set! instvec2id inst-vec last-id)
	      (vector-set! id2inst last-id my-inst)
	      (set! last-id (add1 last-id)))
      (hash-ref instvec2id inst-vec))

    (define (id->real-progs id)
      (for/vector ([x (vector-ref id2prog id)])
		  (vector-ref id2inst x)))

    (define (concat-progs inst-id prog-set)
      (for/set ([prog-id prog-set])
	       (prog->id (cons inst-id (id->prog prog-id)))))

    (define (class-insert-bw-inner! top-hash key-list progs)
      (let* ([first-key (car key-list)]
	     [flag (send enum get-flag first-key)]
	     [live-mask (get-live-mask first-key)])
        ;; (when debug
        ;;       (unless (equal? debug live-mask)
        ;;               (send printer print-syntax
        ;;                     (send printer decode
        ;;                           (vector (vector-ref progs-bw prog))))
        ;;               (raise (format "not-eq ~a ~a" debug live-mask))))
	(unless (hash-has-key? top-hash flag)
		(hash-set! top-hash flag (make-hash)))
	(define middle-hash (hash-ref top-hash flag))
	(unless (hash-has-key? middle-hash live-mask)
		(hash-set! middle-hash live-mask (make-hash)))
	(let ([my-hash (hash-ref middle-hash live-mask)])
	  (for ([key key-list])
	       (if (hash-has-key? my-hash key)
		   (hash-set! my-hash key (set-union (hash-ref my-hash key) progs))
		   (begin
		     (set! c-behaviors-bw (add1 c-behaviors-bw))
		     (hash-set! my-hash key progs)))))))
      

    (define (class-insert-bw! class live test key-list new-progs)
      ;; (pretty-display `(class-insert-bw! ,live ,test ,key-list ,new-progs))
      ;; (pretty-display `(before ,class))
      (define key (cons (sort (car live) <) (sort (cdr live) <)))
      
      ;(set! states-vec (map (lambda (x) (abstract x live-list identity)) states-vec))
      (unless (hash-has-key? class key) 
	      (hash-set! class key (make-vector ce-limit #f)))

      (define tests (hash-ref class key))
      (unless (vector-ref tests test) (vector-set! tests test (make-hash)))
      (class-insert-bw-inner! (vector-ref tests test) key-list new-progs)
      ;; (pretty-display `(after ,class))
      )

    (define (class-init-bw! class live test state-vec)
      (define key (cons (sort (car live) <) (sort (cdr live) <)))

      (unless (hash-has-key? class key)
	      (hash-set! class key (make-vector ce-limit #f)))

      (define tests (hash-ref class key))
      (define top-hash (make-hash))
      (define middle-hash (make-hash))
      (define my-hash (make-hash))

      (vector-set! tests test top-hash)
      (hash-set! top-hash (send enum get-flag state-vec) middle-hash)
      (hash-set! middle-hash (get-live-mask state-vec) my-hash)
      (hash-set! my-hash state-vec (set 0))

      (hash-set! prog2id (list) 0)
      (vector-set! id2prog 0 (list)))

    (define (class-ref-bw class live flag test)
      (vector-ref (hash-ref class live) test))
      

    (define (count-collection x)
      (cond
       [(concat? x) (count-collection (concat-collection x))]
       [(vector? x) 1]
       [(list? x) (foldl + 0 (map count-collection x))]
       [else (raise (format "count-collection: unimplemented for ~a" x))]))

    (define (collect-behaviors x)
      (cond
       [(list? x)  x]
       [(hash? x)
        (let ([ans (list)])
          (for ([val (hash-values x)])
               (set! ans (append (collect-behaviors val) ans)))
          ans)]
       [(box? x) (collect-behaviors (box-val x))]
       [else
        (raise (format "collect-behaviors: unimplemented for ~a" x))]
       ))
    
    (define (get-collection-iterator collection)
      (define ans (list))
      (define (loop x postfix)
        (cond
         [(concat? x)
          (loop (concat-collection x) (vector-append (vector (concat-inst x)) postfix))]
         [(vector? x) 
          (set! ans (cons (vector-append x postfix) ans))]
         [(list? x) 
          (if (empty? x)
              (set! ans (cons postfix ans))
              (for ([i x]) (loop i postfix)))]
         [(set? x) 
          (if (set-empty? x)
              (set! ans (cons postfix ans))
              (for ([i x]) (loop i postfix)))]

         ))
      (loop collection (vector))
      ans)

    (define-syntax-rule (intersect l s)
      (filter (lambda (x) (set-member? s x)) l))
    
    (define t-load 0)
    (define t-build 0)
    (define t-build-inter 0)
    (define t-build-inter2 0)
    (define t-build-hash 0)
    (define t-build-hash2 0)
    (define t-mask 0)
    (define t-hash 0)
    (define t-intersect 0)
    (define t-interpret-0 0)
    (define t-interpret 0)
    (define t-extra 0)
    (define t-verify 0)
    (define c-build-hash 0)
    (define c-build-hash2 0)
    (define c-intersect 0)
    (define c-interpret-0 0)
    (define c-interpret 0)
    (define c-extra 0)
    (define c-check 0)

    (define t-refine 0)
    (define t-collect 0)
    (define t-check 0)

    (define (synthesize-window spec sketch prefix postfix constraint extra 
			       [cost #f] [time-limit 3600]
			       #:hard-prefix [hard-prefix (vector)] 
			       #:hard-postfix [hard-postfix (vector)]
			       #:assume-interpret [assume-interpret #t]
			       #:assume [assumption (send machine no-assumption)])

      (define start-time (current-seconds))
      (define spec-precise spec)
      (define prefix-precise prefix)
      (define postfix-precise postfix)
      (set! spec (reduce-precision spec))
      (set! prefix (reduce-precision prefix))
      (set! postfix (reduce-precision postfix))
      (send printer print-syntax (send printer decode spec))
      
      ;;(define size (if sketch sketch 4))
      (send machine analyze-opcode prefix spec postfix)
      (send machine analyze-args prefix spec postfix #:vreg 0)
      (define live2 (send validator get-live-in postfix constraint extra))
      (define live2-vec (send machine progstate->vector live2))
      (define live1 (send validator get-live-in spec live2 extra))
      (define live1-list (send machine get-operand-live live1))
      (define live2-list (send machine get-operand-live live2))

      (define step-fw 2)
      (define step-bw 1)
      (define ntests 2)
      (define ntests-expand 0)
      (define inits
        (send validator generate-input-states ntests (vector-append prefix spec postfix)
              assumption extra #:db #t))
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector -3 7) (vector) -1 4)
      ;;    (progstate (vector -8 -4) (vector) -1 4)
      ;; 	 ))
      ;; p10
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector -7 5 0) (vector) -1 4)
      ;;    (progstate (vector 5 7 0) (vector) -1 4)
      ;; 	 ))
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector 1 -3 0) (vector) -1 4)
      ;;    (progstate (vector 2 3 0) (vector) -1 4)
      ;; 	 ))
      ;; p11
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector 4 0) (vector) -1 4)
      ;;    (progstate (vector -8 -4) (vector) -1 4)
      ;;    ;; (progstate (vector -6 4) (vector) -1 4)
      ;; 	 ))
      ;; p24
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector 5 0) (vector) -1 4)
      ;;    (progstate (vector 3 0) (vector) -1 4)
      ;;    (progstate (vector -1 0) (vector) -1 4)
      ;;    ))
      ;; p19
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector -6 -5 3 5) (vector) -1 4)
      ;;    (progstate (vector 6 3 4 5) (vector) -1 4)
      ;;    ))
      ;; (define inits
      ;;   (list
      ;;    (progstate (vector -4 3 1 2) (vector) -1 4)
      ;;    (progstate (vector 1 -1 2 -4) (vector) -1 4)
      ;;    ))
      (define states1 
	(map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
      (define states2
	(map (lambda (x) (send simulator interpret spec x #:dep #f)) states1))
      (define states1-vec 
	(map (lambda (x) (send machine progstate->vector x)) states1))
      (define states2-vec 
	(map (lambda (x) (mask-in (send machine progstate->vector x) live2-list #:keep-flag try-cmp)) states2))

      (pretty-display `(states1-vec ,states1-vec))
      (pretty-display `(states2-vec ,states2-vec))
      (pretty-display `(live2-vec ,live2-vec))
      (pretty-display `(live1-list ,live1-list))
      (pretty-display `(live2-list ,live2-list))
      ;;(raise "xxx")
      
      (define ce-in (make-vector ce-limit))
      (define ce-in-vec (make-vector ce-limit))
      (define ce-out-vec (make-vector ce-limit))
      (define ce-count ntests)
      (define ce-count-extra ntests)

      (define ce-in-final (list))
      (define ce-out-vec-final (list))

      (for ([test ntests]
	    [state2-vec states2-vec])
	   (vector-set! ce-out-vec test state2-vec))

      (define prev-classes (make-hash))
      (class-insert! prev-classes live1-list states1-vec (vector))
      (define classes (make-hash))

      (define classes-bw (make-vector (add1 step-bw)))
      (for ([step (add1 step-bw)])
	   (vector-set! classes-bw step (make-hash)))
      
      (define (gen-inverse-behaviors iterator)
        (define p (iterator))
        (define my-inst (car p))
        (when my-inst
          ;(send printer print-syntax (send printer decode my-inst))
          (send inverse gen-inverse-behavior my-inst)
          (gen-inverse-behaviors iterator)
          ))
      
      (gen-inverse-behaviors (send enum reset-generate-inst #f #f #f #f 
				   #f `all #f #:no-args #t))

      (define (check-final p)
        ;; (pretty-display (format "[5] check-final ~a" (length ce-in-final)))
        ;; (send printer print-syntax (send printer decode p))
        (define
          pass
          (for/and ([input ce-in-final]
                    [output-vec ce-out-vec-final])
                   (let* ([my-output 
			   (with-handlers*
			    ([exn? (lambda (e) #f)])
			    (send simulator-precise interpret p input #:dep #f))]
			  [my-output-vec
			   (and my-output (send machine progstate->vector my-output))])
                     (and my-output (send machine state-eq? output-vec my-output-vec live2-vec)))))

        (when
         pass
         (define ce (send validator-precise counterexample 
                          (vector-append prefix-precise spec-precise postfix-precise)
                          (vector-append prefix-precise p postfix-precise)
                          constraint extra #:assume assumption))

         (if ce
             (let* ([ce-input
                     (send simulator-precise interpret prefix-precise ce #:dep #f)]
                    [ce-output
                     (send simulator-precise interpret spec-precise ce-input #:dep #f)]
                    [ce-output-vec
                     (send machine progstate->vector ce-output)])
               (when debug
                     (pretty-display "[6] counterexample (precise)")
                     (send machine display-state ce-input)
                     (pretty-display `(ce-out-vec ,ce-output-vec)))
               (set! ce-in-final (cons ce-input ce-in-final))
               (set! ce-out-vec-final (cons ce-output-vec ce-out-vec-final))
               )
             (begin
               (pretty-display "[7] FOUND!!!")
               (send printer print-syntax (send printer decode p))
               (pretty-display `(ce-count ,ce-count-extra))
               (pretty-display `(ce-count-precise ,(length ce-in-final)))
	       (pretty-display `(time ,(- (current-seconds) start-time)))
               (raise p))))
        )
      
      (define (check-eqv progs progs-bw my-inst my-ce-count)
        (set! c-check (add1 c-check))
        (define t00 (current-milliseconds))
          
        (define (inner-progs p)
          
          ;; (pretty-display "After renaming")
          (when debug
                (pretty-display "[2] all correct")
                (pretty-display `(ce-count-extra ,ce-count-extra))
                )
          (when (= ce-count-extra ce-limit)
                (raise "Too many counterexamples")
                )

          (cond
           [(empty? ce-in-final)
          
            (define ce (send validator counterexample 
                             (vector-append prefix spec postfix)
                             (vector-append prefix p postfix)
                             constraint extra #:assume assumption))

            (if ce
                (let* ([ce-input (send simulator interpret prefix ce #:dep #f)]
                       [ce-input-vec
                        (send machine progstate->vector ce-input)]
                       [ce-output
                        (send simulator interpret spec ce-input #:dep #f)]
                       [ce-output-vec
                        (send machine progstate->vector ce-output)])
                  (when #t
                        (newline)
                        (pretty-display "[3] counterexample")
                        (pretty-display `(ce ,ce-count-extra ,ce-input-vec ,ce-output-vec)))
                  (vector-set! ce-in ce-count-extra ce-input)
                  (vector-set! ce-in-vec ce-count-extra ce-input-vec)
                  (vector-set! ce-out-vec ce-count-extra (mask-in ce-output-vec live2-list #:keep-flag try-cmp))
                  (set! ce-count-extra (add1 ce-count-extra))
                  )
                (begin
                  ;;(pretty-display "[4] found")
                  ;;(send printer print-syntax (send printer decode p))
                  (check-final (increase-precision p))
                  ))]

           [else
            ;;(pretty-display "[4] found")
            ;;(send printer print-syntax (send printer decode p))
            (check-final (increase-precision p))]))

        (define (inner-behaviors p)
          (define t0 (current-milliseconds))
          ;; (pretty-display `(inner-behaviors ,my-ce-count ,ce-count-extra))
	  ;; (send printer print-syntax (send printer decode p))
          
          (define
            pass
            (for/and ([i (reverse (range my-ce-count ce-count-extra))])
                     (let* ([input (vector-ref ce-in i)]
                            [output-vec (vector-ref ce-out-vec i)]
                            [my-output 
			     (with-handlers*
			      ([exn? (lambda (e) #f)])
			      (send simulator interpret p input #:dep #f))]
                            [my-output-vec (and my-output (send machine progstate->vector my-output))])
                       (and my-output
                            (send machine state-eq? output-vec my-output-vec live2-vec)))))
          
          (define t1 (current-milliseconds))
          (set! t-extra (+ t-extra (- t1 t0)))
          (set! c-extra (add1 c-extra))
          (when pass
                (inner-progs p)
                (define t2 (current-milliseconds))
                (set! t-verify (+ t-verify (- t2 t1))))

          )

        (define h1
          (if (= my-ce-count ntests)
              (get-collection-iterator progs)
              progs))

        (define h2
          (if (= my-ce-count ntests)
              (get-collection-iterator progs-bw)
              progs-bw))

        
        ;; (let ([x my-inst])
        ;;   (when (and (equal? `eor
        ;;                      (vector-ref (get-field inst-id machine) (inst-op x)))
        ;;              (equal? `nop 
        ;;                      (vector-ref (get-field shf-inst-id machine) 
        ;;                                  (inst-shfop x)))
        ;;              (equal? 0 (vector-ref (inst-args x) 0))
        ;;              (equal? 0 (vector-ref (inst-args x) 1))
        ;;              (equal? 1 (vector-ref (inst-args x) 2))
        ;;              )
        ;;         (newline)
        ;;         (pretty-display (format "CHECK-EQV ~a ~a" (length h1) (length h2)))))
        
        (define t11 (current-milliseconds))
        
        (for* ([p1 h1]
               [p2 h2])
              (inner-behaviors (vector-append p1 (vector my-inst) p2)))
        (define t22 (current-milliseconds))
        (set! t-collect (+ t-collect (- t11 t00)))
        (set! t-check (+ t-check (- t22 t11)))
        )

      (define (refine my-classes my-classes-bw my-inst my-live1 my-live2 my-flag1 my-flag2)
	(define t00 (current-milliseconds))
        (define cache (make-vector ce-limit))
	(for ([i ce-limit]) 
	     (vector-set! cache i (make-hash)))

        (define (outer my-classes candidates level)
	  ;; (pretty-display `(outer ,level ,candidates))
	  (define my-classes-bw-level (vector-ref my-classes-bw level))
	  (define cache-level (vector-ref cache level))
          (define real-hash my-classes)
                   
	  (when
	   (and (not my-classes-bw-level) (= level ntests-expand))
	   (define t0 (current-milliseconds))
	   (build-hash-bw-all level)
	   (set! my-classes-bw-level 
		 (class-ref-bw (vector-ref classes-bw step-bw) my-live2 my-flag2 level))
	   (define t1 (current-milliseconds))
	   (set! t-build (+ t-build (- t1 t0)))
	   )
                         
          (when (and (list? real-hash) (hash? my-classes-bw-level))
	   ;;(and (list? real-hash) (> (count-collection real-hash) 1))
		;;(pretty-display `(build-fw ,level ,(count-collection real-hash) ,(hash? real-hash-bw)))
                ;; list of programs
                (define t0 (current-milliseconds))
                (set! real-hash (make-hash))
                (define input (vector-ref ce-in level))
                
                (define (loop iterator)
                  (define prog (and (not (empty? iterator)) (car iterator)))
                  (when 
                   prog
                   (let* ([s0 (current-milliseconds)]
                          [state (send simulator interpret prog input #:dep #f)]
                          [state-vec (and state (send machine progstate->vector state))]
                          [s1 (current-milliseconds)])
                     (if (hash-has-key? real-hash state-vec)
                         (hash-set! real-hash state-vec
                                    (cons prog (hash-ref real-hash state-vec)))
                         (hash-set! real-hash state-vec (list prog)))
                     (let ([s2 (current-milliseconds)])
                       (set! t-build-inter (+ t-build-inter (- s1 s0)))
                       (set! t-build-hash (+ t-build-hash (- s2 s1)))
                       (set! c-build-hash (add1 c-build-hash))
                       )
                     )

                   (loop (cdr iterator))
                   ))

                (if (= level ntests)
                    (loop (get-collection-iterator my-classes))
                    (loop my-classes))
                (define t1 (current-milliseconds))
                (set! t-build (+ t-build (- t1 t0)))
                )

          
          (define (inner)
            (define t0 (current-milliseconds))
            (define inters-fw (hash-keys real-hash))
            (define t1 (current-milliseconds))
            (set! t-intersect (+ t-intersect (- t1 t0)))
            (set! c-intersect (add1 c-intersect))

            (for ([inter inters-fw])
              (let ([t0 (current-milliseconds)]
		    [out-vec #f])

		(if (and (> level 0) (hash-has-key? cache-level inter))
		    (set! out-vec (hash-ref cache-level inter))
		    (let ([out 
			   (with-handlers*
			    ([exn? (lambda (e) #f)])
			    (send simulator interpret (vector my-inst) (send machine vector->progstate inter) #:dep #f))])
		      (set! out-vec (and out (mask-in (send machine progstate->vector out) my-live2)))
		      (hash-set! cache-level inter out-vec)))

		(let ([t1 (current-milliseconds)])
		  (set! t-interpret (+ t-interpret (- t1 t0)))
		  (set! c-interpret (add1 c-interpret))
		  (when (= level 0)
			(set! t-interpret-0 (+ t-interpret-0 (- t1 t0)))
			(set! c-interpret-0 (add1 c-interpret-0))))

		(when 
		 out-vec
		 (let ([flag (send enum get-flag out-vec)])
		   (when
		    (hash-has-key? my-classes-bw-level flag)
		    (let* ([s0 (current-milliseconds)]
			   [pairs (hash->list (hash-ref my-classes-bw-level flag))]
			   [s1 (current-milliseconds)])
		      ;; (when (> (length pairs) 1)
                      ;;       (pretty-display `(debug ,level ,pairs))
		      ;;       (raise
		      ;;        (format "(length pairs) = ~a" (length pairs))))
		      (set! t-hash (+ t-hash (- s1 s0)))
		      (for ([pair pairs])
			   (let* ([t0 (current-milliseconds)]
				  [live-mask (car pair)]
				  [classes (cdr pair)]
				  [out-vec-masked 
				   (if (and try-cmp (not (equal? live-mask my-live2)))
				       (mask-in out-vec live-mask)
				       out-vec)]
				  [t1 (current-milliseconds)]
				  [has-key (hash-has-key? classes out-vec-masked)]
				  [progs-set (and has-key (hash-ref classes out-vec-masked))]
				  [t2 (current-milliseconds)]
				  [new-candidates
				   (and has-key
					(if (= level 0)
					    (set->list progs-set)
					    (intersect candidates progs-set)))]
				  [t3 (current-milliseconds)])
			     ;; (pretty-display `(inner ,level ,inter ,out-vec-masked ,new-candidates))
			     ;; (when (>= level 2)
			     ;;       (pretty-display `(result ,classes ,progs-set)))
			     ;; (set! t-mask (+ t-mask (- t1 t0)))
			     (set! t-intersect (+ t-intersect (- t3 t0)))
			     
			     (when
			      (and new-candidates (not (empty? new-candidates)))
			      (if (= 1 (- ce-count level))
				  (begin
				    ;;(pretty-display `(check-eqv-leaf ,level ,ce-count))
				    (check-eqv (hash-ref real-hash inter)
					       (map id->real-progs new-candidates)
					       my-inst ce-count)
				    (set! ce-count ce-count-extra)
				    )
				  (let ([a (outer (hash-ref real-hash inter)
						  new-candidates
						  (add1 level))])
				    (hash-set! real-hash inter a)))))))
		    )))
		)))
            
          (cond
	   [(equal? my-classes-bw-level #f) real-hash]

           [(hash? real-hash)
            (inner)
            real-hash]

           [else
	    (pretty-display `(check-eqv-inter ,level ,real-hash ,candidates))
            (check-eqv (collect-behaviors real-hash)
		       (map id->real-progs candidates)
                       my-inst level)
	    (set! ce-count ce-count-extra)
            real-hash
            ]))
       
        (outer my-classes #f 0)
	(define t11 (current-milliseconds))
	(set! t-refine (+ t-refine (- t11 t00)))
        )


      (define (build-hash my-hash iterator level) 
        ;; Call instruction generator
        (define inst-liveout-vreg (iterator))
        (define my-inst (first inst-liveout-vreg))
	(define my-liveout (second inst-liveout-vreg))

	;; (define my-inst 
	;;   (vector-ref (send printer encode 
	;; 		    (send parser ast-from-string "cmp r0, r1"))
	;; 	      0))
	;; (define my-liveout '(0 1))

        (define cache (make-hash))
        (when 
         my-inst
         ;; (send printer print-syntax-inst (send printer decode-inst my-inst))
	 ;; (pretty-display my-liveout)

         (define (recurse x states2-vec)
           (if (list? x)
               (class-insert! classes my-liveout (reverse states2-vec) (concat x my-inst))
               (for ([pair (hash->list x)])
                    (let* ([state-vec (car pair)]
                           [state (send machine vector->progstate state-vec)]
                           [val (cdr pair)]
                           [out 
                            (if (and (list? val) (hash-has-key? cache state-vec))
                                (hash-ref cache state-vec)
                                (let ([tmp
                                       (with-handlers*
                                        ([exn? (lambda (e) #f)])
                                        (send machine progstate->vector 
                                              (send simulator interpret 
                                                    (vector my-inst)
                                                    state
                                                    #:dep #f)))])
                                  (when (list? val) (hash-set! cache state-vec tmp))
                                  tmp))
                            ])
                      (when out (recurse val (cons out states2-vec)))))))
         
         (recurse my-hash (list))
         (build-hash my-hash iterator level)
	 ))

      (define (build-hash-bw-all test)
	(set! ntests-expand (add1 test))
	(newline)
	(pretty-display `(build-hash-bw-all ,test))
	(class-init-bw! (vector-ref classes-bw 0) live2-list test (vector-ref ce-out-vec test))
	(for ([step step-bw])
	     (newline)
	     (pretty-display `(step-test ,step ,test))
	     (set! c-behaviors-bw 0)
	     (set! c-progs-bw 0)
	     (let ([prev (vector-ref classes-bw step)]
		   [current (vector-ref classes-bw (add1 step))])
	       (for ([pair (hash->list prev)])
		    (let* ([live-list (car pair)]
			   [my-hash (cdr pair)]
                           [flag (hash-keys (vector-ref my-hash 0))]
			   [iterator (send enum reset-generate-inst 
					   #f live-list #f flag
					   #f `all #f #:try-cmp try-cmp)])
		      ;; (pretty-display `(live ,live-list 
                      ;;                        ,(hash-count (vector-ref my-hash test))
                      ;;                        ,(hash-count (car (hash-values (vector-ref my-hash test))))))
                      (build-hash-bw test current live-list my-hash iterator)
                      )))
	     (pretty-display `(behavior-bw ,test ,step ,c-behaviors-bw ,c-progs-bw ,(- (current-seconds) start-time)))
	     )
	)

      (define (build-hash-bw test current old-liveout my-hash iterator)
	(define my-hash-test (vector-ref my-hash test))
	(define (inner)
	  (define inst-liveout-vreg (iterator))
	  (define my-inst (first inst-liveout-vreg))
	  (define my-liveout (third inst-liveout-vreg))

	  ;; (define my-inst 
	  ;;   (vector-ref (send printer encode 
	  ;; 		    (send parser ast-from-string "movne r3, 0"))
	  ;; 	      0))
	  ;; (define my-liveout (cons (list 3) (list)))

	  (when my-inst
		;; (send printer print-syntax-inst (send printer decode-inst my-inst))
                ;; (pretty-display `(live ,my-liveout))
                (define inst-id (inst->id my-inst))
                ;; (define t-interpret 0)
                ;; (define t-hash 0)
                ;; (define c 0)
		(for* ([live2states (hash-values my-hash-test)]
                       [mapping (hash-values live2states)]
		       [pair (hash->list mapping)])
		      (let* ([out-vec (car pair)]
			     [progs (cdr pair)]
                             ;;[t0 (current-milliseconds)]
			     [in-vec (send inverse interpret-inst my-inst out-vec old-liveout)]
                             ;;[t1 (current-milliseconds)]
                             )
			;; (pretty-display `(test-live ,test ,my-liveout ,in-vec))
			(when (and in-vec (not (empty? in-vec)))
			      (class-insert-bw! current my-liveout test 
						in-vec (concat-progs inst-id progs)))
                        ;; (let ([t2 (current-milliseconds)])
                        ;;   (set! t-interpret (+ t-interpret (- t1 t0)))
                        ;;   (set! t-hash (+ t-hash (- t2 t1)))
                        ;;   (when (list? in-vec) (set! c (+ c (length in-vec))))
                        ;;   )
                        )
                      )
                ;; (pretty-display `(time ,t-interpret ,t-hash ,c))
		(inner)
		))
	(inner))

      ;; Grow forward
      (for ([i step-fw])
        (newline)
        (pretty-display `(grow ,i))
        (set! c-behaviors 0)
        (set! c-progs 0)
      	(for ([pair (hash->list prev-classes)])
      	     (let* ([key (car pair)]
        	    [live-list (entry-live key)]
        	    [flag (entry-flag key)]
      		    [my-hash (cdr pair)]
      		    [iterator (send enum reset-generate-inst 
        			    live-list #f flag #f
        			    #f `all #f #:try-cmp try-cmp)])
               (pretty-display `(live ,live-list ,flag))
      	       (build-hash my-hash iterator i)))
        (set! prev-classes classes)
        (set! classes (make-hash))
        (pretty-display `(behavior ,i ,c-behaviors ,c-progs ,(- (current-seconds) start-time)))
        )

      ;; Grow backward
      (for ([test ntests])
           (build-hash-bw-all test))

      (define middle 0)
      (define (refine-all hash1 live1 flag1 hash2 live2 flag2 iterator)
	(define inst-liveout-vreg (iterator))
        (define my-inst (first inst-liveout-vreg))
	;; (define my-inst 
	;;   (vector-ref (send printer encode 
	;; 		    (send parser ast-from-string "cmp r2, r3"))
	;; 	      0))
        (when 
            my-inst
          (send printer print-syntax-inst (send printer decode-inst my-inst))
	  (set! middle (add1 middle))
          (define ttt (current-milliseconds))
          (refine hash1 hash2 my-inst live1 live2 flag1 flag2)
	  (when 
	   (> (- (current-milliseconds) ttt) 100)
	   (pretty-display (format "search ~a ~a = ~a + ~a + ~a | ~a\t(~a + ~a/~a + ~a + ~a/~a)\t~a ~a ~a/~a\t[~a/~a]\t~a/~a\t~a/~a (~a) ~a" 
	  			   (- (current-milliseconds) ttt) ce-count-extra
				   t-refine t-collect t-check
	  			   t-build t-build-inter t-build-hash c-build-hash t-build-inter2 t-build-hash2 c-build-hash2
	  			   t-mask t-hash t-intersect c-intersect
	  			   t-interpret-0 c-interpret-0
	  			   t-interpret c-interpret
	  			   t-extra c-extra c-check
	  			   t-verify
	  			   )))
          (set! t-build 0) (set! t-build-inter 0) (set! t-build-inter2 0) (set! t-build-hash 0) (set! t-build-hash2 0) (set! t-mask 0) (set! t-hash 0) (set! t-intersect 0) (set! t-interpret-0 0) (set! t-interpret 0) (set! t-extra 0) (set! t-verify 0)
          (set! c-build-hash 0) (set! c-build-hash2 0) (set! c-intersect 0) (set! c-interpret-0 0) (set! c-interpret 0) (set! c-extra 0) (set! c-check 0)
          (set! t-refine 0) (set! t-collect 0) (set! t-check 0)
          (refine-all hash1 live1 flag1 hash2 live2 flag2 iterator)
	  ))

      ;; (pretty-display "problematic program:")
      ;; (send printer print-syntax (send printer decode (id->real-progs 343)))

      (define keys (hash-keys prev-classes))
      ;; (for ([key keys])
      ;;      (pretty-display `(key ,(car key) ,(cdr key))))
      (set! keys (sort keys (lambda (x y) (> (length (car (entry-live x))) (length (car (entry-live y)))))))

      (define order 0)

      ;; Search
      (define ttt (current-milliseconds))
      (for* ([key1 keys]
             [pair2 (hash->list (vector-ref classes-bw step-bw))])
            ;; (pretty-display `(search ,key1 ,pair2))
           (let* ([flag1 (entry-flag key1)]
      		  [live1 (entry-live key1)]
                  [my-hash1 (hash-ref prev-classes key1)]
                  [live2 (car pair2)]
                  [my-hash2 (cdr pair2)]
		  [flag2 (hash-keys (vector-ref my-hash2 0))]
                  [pass (for/and ([i ntests]) (vector-ref my-hash2 i))])
             (when
              pass
              (let ([iterator
                     (send enum reset-generate-inst 
                           live1 live2 flag1 flag2
                           #f `all #f #:try-cmp try-cmp)])
                (newline)
                (pretty-display `(refine ,order ,live1 ,flag1 ,live2))
                ;;(pretty-display `(hash ,(vector-ref my-hash2 0) ,(vector-ref my-hash2 1)))
                ;; (when (and (equal? live1 '(0 1)) (equal? live2 '()))
                ;;       (pretty-display "===================")
                      (refine-all my-hash1 live1 flag1 my-hash2 live2 #f iterator)
                      ;; )
                (pretty-display `(middle-count ,middle))
                (set! order (add1 order))
                ))))
      (pretty-display `(time ,(- (current-seconds) start-time)))

      )
    ))
