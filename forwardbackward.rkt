#lang racket

(require "inst.rkt" "decomposer.rkt" "ops-racket.rkt" "enumerator.rkt"
         "special.rkt" "memory-racket.rkt" "queue-racket.rkt")
(require racket/generator)

(provide forwardbackward% entry-live)

(struct concat (collection inst))
(struct box (val))

(define-syntax-rule (entry live prune) (list live prune))
(define-syntax-rule (entry-live x) (first x))
(define-syntax-rule (entry-prune x) (second x))

;; Enuemrative search performas bidirectional search.
;; It uses two main data structures to memorize programs that have been explore,
;; one for forward direction and another for backward direction.
;;
;; For forward direction: "classes"
;;  classes     := (entry live1 pruning-info) -> nested hash
;;                 where live1 is liveness from (udpate-live) method,
;;                 and pruning-info is from (get-pruning-info) method
;;  nested hash := progstate -> nested hash
;;                 | set of programs
;;
;; For backward direction: "classes-bw"
;;  classes-bw := (vector hash1 ...), where index = backward step
;;  hash1      := live1 -> vector2, where live1 is liveness from (udpate-live) method
;;  vector2    := (vector hash2 ...), where idnex = test ID
;;  hash2      := pruning-info -> live2 -> progstate -> set of programs
;;                where pruning-info is from (get-pruning-info) method,
;;                and live2 is from (get-live-mask) method

(define forwardbackward%
  (class decomposer%
    (super-new)
    (inherit-field machine printer simulator validator stat syn-mode)
    (inherit window-size)
    (init-field inverse% enumerator% [enum #f])
    (override synthesize-window superoptimize-linear superoptimize-binary)
    (public try-cmp? combine-live sort-live sort-live-bw
            reduce-precision increase-precision
            reduce-precision-assume
            change-inst change-inst-list
            mask-in get-live-mask prescreen)

    (define (debug-inst my-inst) #f)
      ;;(and (equal? (inst-op my-inst) 10)))
    
    (define debug #f)
    (define verbo #f)
    (define info #f)
    (define ce-limit 100)

    ;; Actual bitwidth
    (define bit-precise (get-field bitwidth machine))
    
    ;; Reduce bitwidth
    (define bit 4)
    (define mask (sub1 (arithmetic-shift 1 bit)))
    (define mask-1 (sub1 (arithmetic-shift 1 (sub1 bit))))

    (set! machine
          (new (send machine get-constructor) [bitwidth bit]
               [config (send machine get-config)]))
    (define simulator-abst
      (new (send simulator get-constructor) [machine machine]))
    (define validator-abst
      (new (send validator get-constructor) [machine machine]
           [simulator
            (new (send (get-field simulator validator) get-constructor)
                 [machine machine])]))
    (define inverse (new inverse% [machine machine] [simulator simulator-abst]))
    (set! enum (new enumerator% [machine machine] [printer printer]))
    
    ;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;
    (define (prescreen my-inst state-vec) #t)
    
    ;; Return a copy of a given instruction x,
    ;; but replacing each constant c in the instruction x with (change c).
    (define (change-inst x change)
      (define opcode-id (inst-op x))
      (define args (inst-args x))
      (define types (send machine get-arg-types opcode-id))
      
      (define new-args
        (for/vector
         ([arg args]
          [type types])
         (if (member type '(const bit bit-no-0 op2))
             (change arg type)
             arg)))

      (inst (inst-op x) new-args))

    ;; Return a list of copies of a given instruction x,
    ;; but replacing each constant c in the instruction x with
    ;; one of the values from (change c).
    ;; Because (change c) returns a list of values instead of a value,
    ;; this method has to return all possible unique copies of x.
    (define (change-inst-list x change)
      (define op (inst-op x))
      (define args (inst-args x))
      (define types (send machine get-arg-types op))
      
      (define new-args
        (for/list
         ([arg args]
          [type types])
         (if (member type '(const bit bit-no-0 op2))
             (change arg type)
             (list arg))))

      (for/list ([final-args (all-combination-list new-args)])
                (inst op (list->vector final-args))))

    ;; Heuristic to sort what programs to explore first.
    (define (sort-live x) x)
    (define (sort-live-bw x) x)
    (define (reduce-precision-assume x) x)
    (define (mask-in state live #:keep-flag [keep #t])
      ;;(pretty-display `(mask-in ,state ,live))
      (define (f state live)
        (cond
         [(and (list? state) (number? live))
          (for/list ([s state]
                     [l (in-naturals)])
                    (and (< l live) s))]
         [(and (vector? state) (number? live))
          (for/vector ([s state]
                       [l (in-naturals)])
                      (and (< l live) s))]
         [(is-a? state memory-racket%)
          (if live state (send state clone-init))]
         [(is-a? state special%) state]
         [(boolean? live) (and live state)] ;; TODO: add this case
         [(list? state)
          (for/list ([s state]
                     [l live])
                    (f s l))]
         [(vector? state)
          (for/vector ([s state]
                       [l live])
                      (f s l))]
         [(pair? state)
          (cons (f (car state) (car live)) (f (cdr state) (cdr live)))]
         [else (and live state)]))
      (if live
          (f state live)
          state))

    (define (get-live-mask state)
      (cond
       [(list? state) (for/list ([s state]) (get-live-mask s))]
       [(vector? state) (for/vector ([s state]) (get-live-mask s))]
       [(pair? state)
        (cons (get-live-mask (car state)) (get-live-mask (cdr state)))]
       [(is-a? state special%) (send state get-live-mask)]
       [else (number? state)]))

    (define c-behaviors 0)
    (define c-progs 0)

    ;; Insert state-vec into forward equivlance classes.
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

      (define key (entry live (send enum get-pruning-info (car states-vec))))
      (unless (hash-has-key? class key) (hash-set! class key (make-hash)))
      (insert-inner (hash-ref class key) states-vec prog))

    (define c-behaviors-bw 0)
    (define c-progs-bw 0)

    ;; Use IDs to represent programs and instructions to use less memory.
    (define instvec2id (make-hash))
    (define id2inst (make-vector 1000000 #f))
    (define last-id 0)

    (define prog2id (make-hash))
    (define id2prog (make-vector 10000000 #f))
    (define last-prog-id 1)

    (define (reset)
      (set! instvec2id (make-hash))
      (set! id2inst (make-vector 1000000 #f))
      (set! last-id 0)
      (set! prog2id (make-hash))
      (set! last-prog-id 1))

    (define (prog->id prog)
      (unless (hash-has-key? prog2id prog)
	      (hash-set! prog2id prog last-prog-id)
	      (vector-set! id2prog last-prog-id prog)
	      (set! c-progs-bw (add1 c-progs-bw))
	      (set! last-prog-id (add1 last-prog-id)))
      (hash-ref prog2id prog))

    (define (id->prog id) (vector-ref id2prog id))
    (define (inst->vector x) (vector (inst-op x) (inst-args x)))

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

    
    ;; Insert into backward equivlance classes.
    (define (class-insert-bw-inner! top-hash state progs)
      (let* ([prune (send enum get-pruning-info state)]
             [live-mask (get-live-mask state)])
        (unless live-mask
                (pretty-display `(state ,state))
                (raise "get-live-mask returns #f"))
        
        (unless (hash-has-key? top-hash prune)
        	(hash-set! top-hash prune (make-hash)))
        (define middle-hash (hash-ref top-hash prune))
        (unless (hash-has-key? middle-hash live-mask)
        	(hash-set! middle-hash live-mask (make-hash)))
        (let ([my-hash (hash-ref middle-hash live-mask)])
          (if (hash-has-key? my-hash state)
              (hash-set! my-hash state (set-union (hash-ref my-hash state) progs))
              (begin
                (set! c-behaviors-bw (add1 c-behaviors-bw))
                (hash-set! my-hash state progs))))))
      

    (define (class-insert-bw! class live test state new-progs)
      ;; (pretty-display `(class-insert-bw! ,live ,test ,state ,new-progs))
      ;; (pretty-display `(before ,class))
      (define key live)
      
      ;(set! states-vec (map (lambda (x) (abstract x live-list identity)) states-vec))
      (unless (hash-has-key? class key) 
              (hash-set! class key (make-vector ce-limit #f)))

      (define tests (hash-ref class key))
      (unless (vector-ref tests test) (vector-set! tests test (make-hash)))
      (class-insert-bw-inner! (vector-ref tests test) state new-progs)
      ;; (pretty-display `(after ,class))
      )

    (define (class-init-bw! class live test state-vec)
      (define key live)

      (unless (hash-has-key? class key)
	      (hash-set! class key (make-vector ce-limit #f)))

      (define tests (hash-ref class key))
      (define top-hash (make-hash))
      (define middle-hash (make-hash))
      (define my-hash (make-hash))

      (vector-set! tests test top-hash)
      (hash-set! top-hash (send enum get-pruning-info state-vec) middle-hash)
      (hash-set! middle-hash (get-live-mask state-vec) my-hash)
      (hash-set! my-hash state-vec (set 0))

      (hash-set! prog2id (list) 0)
      (vector-set! id2prog 0 (list)))

    (define (class-ref-bw class live test)
      (vector-ref (hash-ref class live) test))
      
    ;; Count number of programs in x.
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

    (define (copy x)
      (define ret (make-hash))
      (for ([pair (hash->list x)])
           (let ([key (car pair)]
                 [val (cdr pair)])
             (if (list? val)
                 (hash-set! ret key val)
                 (hash-set! ret key (copy val)))))
      ret)

    (define-syntax-rule (intersect l s)
      (filter (lambda (x) (set-member? s x)) l))


    (define (try-cmp? code state live) 0)
    ;; Combine liveness from two sources
    ;; 1. x is from using update-live from live-in.
    ;;    This allows the optimizer to use intermediate registers more flexibly.
    ;;    Use x if update-live is precise.
    ;; 2. y is from using symbolic analyze from live-out.
    ;;    This may disallow the optimizer to use some intermedates.
    ;;    If r0 and mem0 have the same value, if the spec use mem0 in that window
    ;;    then r0 won't be live in y (but it is in x).
    (define (combine-live x y) x)
    
    ;;;;;;;;;;;;;;;;;;;;;;; Reduce/Increase bitwidth ;;;;;;;;;;;;;;;;;;;;;;

    ;; Convert input program into reduced-bitwidth program by replacing constants.
    ;; output: a pair of (reduced-bitwidth program, replacement map*)
    ;;   *replacement map maps reduced-bitwidth constants to sets of actual constants.
    (define (reduce-precision prog)
      ;; TODO: common one
      (define mapping (make-hash))
      (define (change arg type)
        (define (inner)
          (cond
           [(member type '(op2 bit bit-no-0))
            (cond
             [(and (> arg 0) (<= arg (/ bit-precise 4)))
              (/ bit 4)]
             [(and (> arg (/ bit-precise 4)) (< arg (* 3 (/ bit-precise 4))))
              (/ bit 2)]
             [(and (>= arg (* 3 (/ bit-precise 4))) (< arg bit-precise))
              (* 3 (/ bit 4))]
             [(= arg bit-precise) bit]
             [(> arg 0) (bitwise-and arg mask-1)]
             [else (finitize (bitwise-and arg mask) bit)])]

           [(> arg 0) (bitwise-and arg mask-1)]
           [else (finitize (bitwise-and arg mask) bit)]))

        (define ret (inner))
        (if (hash-has-key? mapping ret)
            (let ([val (hash-ref mapping ret)])
              (unless (member arg val)
                      (hash-set! mapping ret (cons arg val))))
            (hash-set! mapping ret (list arg)))
        ret)
        
      (cons (for/vector ([x prog]) (change-inst x change)) mapping))
    
    ;; Convert reduced-bitwidth program into program in precise domain.
    ;; prog: reduced bitwidth program
    ;; mapping: replacement map returned from 'reduce-precision' function
    ;; output: a list of potential programs in precise domain
    (define (increase-precision prog mapping)
      (define (change arg type)
        (define (finalize x)
          (if (hash-has-key? mapping arg)
              (let ([val (hash-ref mapping arg)])
                (if (member x val) val (cons x val)))
              (if (= x -2)
                  (list -2 -8)
                  (list x))))
        (define ret
          (cond
           [(= arg bit) (finalize bit-precise)]
           [(= arg (sub1 bit)) (finalize (sub1 bit-precise))]
           [(= arg (/ bit 2)) (finalize (/ bit-precise 2))]
           [else (finalize arg)]))

        (if (member arg ret)
            ret
            (cons arg ret)))

      (define ret (list))
      (define (recurse lst final)
        (if (empty? lst)
            (set! ret (cons (list->vector final) ret))
            (for ([x (car lst)])
                 (recurse (cdr lst) (cons x final)))))
      
      (recurse (reverse (for/list ([x prog]) (change-inst-list x change)))
               (list))
      ret)


    ;;;;;;;;;;;;;;;;;;;;;;; Timing variables ;;;;;;;;;;;;;;;;;;;;;;
    (define t-build 0)
    (define t-build-inter 0)
    (define t-build-hash 0)
    (define t-mask 0)
    (define t-hash 0)
    (define t-intersect 0)
    (define t-interpret-0 0)
    (define t-interpret 0)
    (define t-extra 0)
    (define t-verify 0)
    (define t-ce-abst 0)
    (define c-build-hash 0)
    (define c-mask 0)
    (define c-intersect 0)
    (define c-interpret-0 0)
    (define c-interpret 0)
    (define c-extra 0)
    (define c-check 0)
    (define c-verify 0)
    (define c-ce-abst 0)

    (define t-refine 0)
    (define t-collect 0)
    (define t-check 0)

    (define start-time #f)
    
    ;;;;;;;;;;;;;;;;;;;;;;; Main functions ;;;;;;;;;;;;;;;;;;;;;;
    (define (superoptimize-binary spec constraint time-limit size 
				  #:lower-bound [lower-bound 0]
                                  #:assume [assumption (send machine no-assumption)]
                                  #:prefix [prefix (vector)] #:postfix [postfix (vector)]
                                  #:hard-prefix [hard-prefix (vector)] 
                                  #:hard-postfix [hard-postfix (vector)]
                                  )
      (superoptimizer-common spec prefix postfix constraint time-limit size
                             assumption))

    (define (superoptimize-linear spec constraint time-limit size
			   #:assume [assumption (send machine no-assumption)]
                           #:prefix [prefix (vector)] #:postfix [postfix (vector)]
                           #:hard-prefix [hard-prefix (vector)]
                           #:hard-postfix [hard-postfix (vector)])
      (superoptimizer-common spec prefix postfix constraint time-limit size
                             assumption))

    (define (superoptimizer-common spec prefix postfix constraint time-limit size assumption)
      (define sketch (make-vector (vector-length spec)))

      (synthesize-window spec sketch prefix postfix constraint 
                         (send simulator performance-cost spec) time-limit
                         #:assume assumption))

    (define (synthesize-window spec sketch prefix postfix constraint  
			       [cost #f] [time-limit 3600]
			       #:hard-prefix [hard-prefix (vector)] 
			       #:hard-postfix [hard-postfix (vector)]
			       #:assume [assumption (send machine no-assumption)])
      (set! start-time (current-seconds))
      (send machine reset-opcode-pool)
      (send machine analyze-opcode prefix spec postfix)
      (define init
        (car (send validator
                   generate-input-states 1 (vector-append prefix spec postfix)
                   assumption #:db #t)))
      (define state2
        (send simulator interpret
              (vector-append prefix spec) init))
      (define live2
        (send validator get-live-in postfix constraint))
      (define try-cmp-status (try-cmp? spec state2 live2))
      (when info (pretty-display `(status ,try-cmp-status)))

      (define out-program #f)
      (define (exec x)
        (when (vector? out-program)
              (set! cost (send simulator performance-cost out-program)))
        
        (define iterator
          (generator
           ()
           (synthesize spec sketch prefix postfix constraint cost
                       assumption x time-limit)))
        
        (define (loop best-p)
          (define p (iterator))
          (when info (pretty-display `(loop-get ,p)))

          (cond
           [(equal? p "timeout") (or best-p p)]
           [p (loop p)]
           [else best-p]))
        
        (define tmp (loop #f))
        (set! out-program
              (if (vector? tmp) tmp (or out-program tmp))))
      
      (cond
       [(= try-cmp-status 0) ;; don't try cmp
        (exec #f)]
       [(= try-cmp-status 1) ;; must try cmp
        (exec #t)]
       [(= try-cmp-status 2) ;; should try cmp
        (exec #f)
        (set! start-time (current-seconds))
        (exec #t)
        ])

      out-program
      )

    (define (synthesize spec sketch prefix postfix constraint cost
                        assumption try-cmp time-limit)
      (collect-garbage)
      (define size-from sketch)
      (define size-to sketch)
      (when (vector? sketch)
            (let ([len (vector-length sketch)])
              (set! size-from 1)
              (set! size-to (min len (window-size)))))
      
      (reset)
      (send machine reset-arg-ranges)
      (define spec-precise spec)
      (define prefix-precise prefix)
      (define postfix-precise postfix)
      (define assumption-precise assumption)
      (define abst2precise #f)

      (let ([tmp (reduce-precision spec)])
        (set! spec (car tmp))
        (set! abst2precise (cdr tmp)))
      (when debug
            (pretty-display `(try-cmp ,try-cmp))
            (pretty-display `(abst2precise ,abst2precise)))
      
      (set! prefix (car (reduce-precision prefix)))
      (set! postfix (car (reduce-precision postfix)))
      ;; (pretty-display `(assume ,assumption))
      (set! assumption (reduce-precision-assume assumption))

      ;; (send machine display-state assumption-precise)
      ;; (newline)
      ;; (send machine display-state assumption)

      (display "[")
      (send printer print-syntax (send printer decode prefix))
      (pretty-display "]")
      (send printer print-syntax (send printer decode spec))
      (display "[")
      (send printer print-syntax (send printer decode postfix))
      (pretty-display "]")

      (define live3-vec (send machine progstate->vector constraint))
      (define live2 (send validator-abst get-live-in postfix constraint))
      (define live2-vec (send machine progstate->vector live2))
      (define live1 (send validator-abst get-live-in (vector-append spec postfix) constraint))
      (define live0 (send validator-abst get-live-in (vector-append prefix spec postfix) constraint))
      (define live0-list (send machine progstate->vector live0))

      (define live1-list-alt live0-list)
      (for ([x prefix])
           (set! live1-list-alt (send machine update-live live1-list-alt x)))
      
      (send machine analyze-args prefix spec postfix live1-list-alt live2)

      ;; Convert live2 after analyze-args to filter some live-out regs
      ;; that do not involve in here.
      (define live1-list (send machine progstate->vector live1))
      (define live2-list (send machine progstate->vector live2))
      
      (set! live1-list (combine-live live1-list-alt live1-list))

      (define step-bw 0)
      (define step-fw 0)
      (define step-bw-max 3)
      
      (define ntests 2)
      (define inits
        (send validator-abst generate-input-states ntests (vector-append prefix spec postfix)
              assumption #:db #t))
      (define states1 
	(map (lambda (x) (send simulator-abst interpret prefix x)) inits))
      (define states2
	(map (lambda (x) (send simulator-abst interpret spec x)) states1))
      (define states1-vec 
	(map (lambda (x) (mask-in (send machine progstate->vector x) live1-list))
             states1))
      (define states2-vec 
	(map (lambda (x) (mask-in (send machine progstate->vector x) live2-list #:keep-flag try-cmp)) states2))

      (when debug
            (pretty-display `(states1-vec ,states1-vec))
            (pretty-display `(states2-vec ,states2-vec))
            (pretty-display `(live2-vec ,live2-vec))
            (pretty-display `(live1-list ,live1-list))
            (pretty-display `(live2-list ,live2-list)))
      
      (define ce-in (make-vector ce-limit))
      (define ce-out (make-vector ce-limit))
      (define ce-in-vec (make-vector ce-limit))
      (define ce-out-vec (make-vector ce-limit))
      (define ce-count ntests)
      (define ce-count-extra ntests)

      (define ce-in-final (list))
      (define ce-out-final (list))
      (define ce-out-vec-final (list))

      (for ([test ntests]
            [state1 states1]
            [state1-vec states1-vec]
            [state2 states2]
	    [state2-vec states2-vec])
           (vector-set! ce-in test state1)
	   (vector-set! ce-in-vec test state1-vec)
           (vector-set! ce-out test state2)
	   (vector-set! ce-out-vec test state2-vec))

      ;; Initialize forward and backward classes
      (define prev-classes (make-hash))
      (class-insert! prev-classes live1-list states1-vec (vector))
      (define classes (copy prev-classes))

      (define classes-bw (make-vector (add1 step-bw-max)))
      (define classes-bw-expand (make-vector (add1 step-bw-max) 0))
      (for ([step (add1 step-bw-max)])
	   (vector-set! classes-bw step (make-hash)))
      (for ([test ntests])
           (class-init-bw! (vector-ref classes-bw 0) live2-list test (vector-ref ce-out-vec test))
           )
      (vector-set! classes-bw-expand 0 ntests)
      
      (define (gen-inverse-behaviors iterator)
        (define p (iterator))
        (define my-inst (car p))
        (when my-inst
          ;; (send printer print-struct my-inst)
          ;; (send printer print-syntax (send printer decode my-inst))
          (send inverse gen-inverse-behavior my-inst)
          (gen-inverse-behaviors iterator)
          ))

      (pretty-display "Generate inverse behaviors...")
      (gen-inverse-behaviors (send enum generate-inst #f #f #f #f 
				   #:no-args #t #:try-cmp try-cmp))
      (pretty-display "Finish.")

      (define (check-final p)
        (when debug
              (pretty-display (format "[5] check-final ~a" (length ce-in-final)))
              (send printer print-syntax (send printer decode p)))
        (define t1 (current-milliseconds))
        (define
          pass
          (for/and ([input ce-in-final]
                    [output ce-out-final]
                    [output-vec ce-out-vec-final])
                   (let* ([my-output 
			   (with-handlers*
			    ([exn? (lambda (e) #f)])
			    (send simulator interpret (vector-append p postfix-precise)
                                  input output))]
			  [my-output-vec
			   (and my-output (send machine progstate->vector my-output))])
                     (and my-output (send machine state-eq? output-vec my-output-vec live3-vec)))))
        (define t2 (current-milliseconds))
        (set! t-ce-abst (+ t-ce-abst (- t2 t1)))
        (set! c-ce-abst (add1 c-ce-abst))
            
        (define final-program (vector-append prefix-precise p postfix-precise))

        (when
         pass
         (define ce (send validator counterexample 
                          (vector-append prefix-precise spec-precise postfix-precise)
                          final-program
                          constraint #:assume assumption))

         (if ce
             (let* ([ce-input
                     (send simulator interpret prefix-precise ce)]
                    [ce-output
                     (send simulator interpret (vector-append spec-precise postfix-precise) ce-input)]
                    [ce-output-vec
                     (send machine progstate->vector ce-output)])
               (when #t
                     (pretty-display "[6] counterexample (precise)")
                     (send machine display-state ce-input)
                     ;;(pretty-display `(ce-out-vec ,ce-output-vec))
                     )
               (set! ce-in-final (cons ce-input ce-in-final))
               (set! ce-out-final (cons ce-output ce-out-vec-final))
               (set! ce-out-vec-final (cons ce-output-vec ce-out-vec-final))
               )
             (let ([final-cost
                    (send simulator performance-cost p)])
               (newline)
               (pretty-display "[7] FOUND!!!")
               (send printer print-syntax (send printer decode p))
               (newline)
               (pretty-display `(cost ,final-cost))
               (pretty-display `(ce-count ,ce-count-extra))
               (pretty-display `(ce-count-precise ,(length ce-in-final)))
	       ;;(pretty-display `(time ,(- (current-seconds) start-time)))
               (newline)

               ;; Print to file
               (send stat update-best-correct
                     final-program
                     (send simulator performance-cost final-program))
               (yield p)
               (unless (member syn-mode '(linear binary))
                       (yield #f))
               (set! cost final-cost)
               (set! start-time (current-seconds))

               (when (<= final-cost (vector-length p))
                     (when info (pretty-display "YIELD done early"))
                     (yield #f))
               )))
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

            (define ce (send validator-abst counterexample 
                             (vector-append prefix spec postfix)
                             (vector-append prefix p postfix)
                             constraint #:assume assumption))

            (if ce
                (let* ([ce-input (send simulator-abst interpret prefix ce)]
                       [ce-input-vec
                        (mask-in (send machine progstate->vector ce-input) live1-list)]
                       [ce-output
                        (send simulator-abst interpret spec ce-input)]
                       [ce-output-vec
                        (mask-in (send machine progstate->vector ce-output) live2-list
                                 #:keep-flag try-cmp)])
                  (when debug
                        (newline)
                        (pretty-display "[3] counterexample")
                        ;;(pretty-display `(ce ,ce-count-extra ,ce-input-vec ,ce-output-vec))
                        )
                  (vector-set! ce-in ce-count-extra ce-input)
                  (vector-set! ce-out ce-count-extra ce-output)
                  (vector-set! ce-in-vec ce-count-extra ce-input-vec)
                  (vector-set! ce-out-vec ce-count-extra ce-output-vec)
                  (set! ce-count-extra (add1 ce-count-extra))
                  )
                (begin
                  (when debug
                        (pretty-display "[4] found")
                        (send printer print-syntax (send printer decode p)))
                  (for ([x (increase-precision p abst2precise)])
                       (check-final x))
                  ))]

           [else
            (when debug
                  (pretty-display "[4] found")
                  (send printer print-syntax (send printer decode p)))
            (for ([x (increase-precision p abst2precise)])
                 (check-final x))
            ]))

        (define (inner-behaviors p)
          (define t0 (current-milliseconds))
          
          (define
            pass
            (and
             (or (not cost) (< (send simulator-abst performance-cost p) cost))
             (for/and ([i (reverse (range my-ce-count ce-count-extra))])
                      (let* ([input (vector-ref ce-in i)]
                             [output (vector-ref ce-out i)]
                             [output-vec (vector-ref ce-out-vec i)]
                             [my-output 
                              (with-handlers*
                               ([exn? (lambda (e) #f)])
                               (send simulator-abst interpret p input output))]
                             [my-output-vec (and my-output (send machine progstate->vector my-output))])
                        (and my-output
                             (send machine state-eq? output-vec my-output-vec live2-vec))))))
          
          (define t1 (current-milliseconds))
          (set! t-extra (+ t-extra (- t1 t0)))
          (set! c-extra (add1 c-extra))
          (when pass
                (inner-progs p)
                (define t2 (current-milliseconds))
                (set! c-verify (add1 c-verify))
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
        
        (define t11 (current-milliseconds))
        
        (for* ([p1 h1]
               [p2 h2])
              (inner-behaviors (vector-append p1 (vector my-inst) p2)))
        (define t22 (current-milliseconds))
        (set! t-collect (+ t-collect (- t11 t00)))
        (set! t-check (+ t-check (- t22 t11)))
        )

      (define (refine my-classes my-classes-bw my-inst my-live1 my-live2)
	(define t00 (current-milliseconds))
        (define cache (make-vector ce-limit))
	(for ([i ce-limit]) 
	     (vector-set! cache i (make-hash)))

        (define (outer my-classes candidates level)
	  (when (debug-inst my-inst)
                (newline)
	  	(pretty-display `(outer ,level ,candidates)))
	  (define my-classes-bw-level (vector-ref my-classes-bw level))
	  (define cache-level (vector-ref cache level))
          (define real-hash my-classes)
                   
	  (when
	   (and (not my-classes-bw-level)
                (= level (vector-ref classes-bw-expand step-bw)))
	   (define t0 (current-milliseconds))
	   (build-hash-bw-all level)
	   (set! my-classes-bw-level 
		 (class-ref-bw (vector-ref classes-bw step-bw) my-live2 level))
	   (define t1 (current-milliseconds))
	   (set! t-build (+ t-build (- t1 t0)))
	   )
                         
          (define ce-out-level (vector-ref ce-out level))
          (when (and (list? real-hash) (hash? my-classes-bw-level))
	   ;;(and (list? real-hash) (> (count-collection real-hash) 1))
		;;(pretty-display `(build-fw ,level ,(count-collection real-hash) ,(hash? real-hash-bw)))
                ;; list of programs
                (define t0 (current-milliseconds))
                (set! real-hash (make-hash))
                (define input (send machine vector->progstate (vector-ref ce-in-vec level)))
                
                (define (loop iterator)
                  (define prog (and (not (empty? iterator)) (car iterator)))
                  (when 
                   prog
                   (let* ([s0 (current-milliseconds)]
                          [state
			   (with-handlers*
			    ([exn? (lambda (e) #f)])
                            (send simulator-abst interpret prog input ce-out-level))]
                          [state-vec (and state (send machine progstate->vector state))]
                          [s1 (current-milliseconds)])
                     (when
                      state-vec
                      (if (hash-has-key? real-hash state-vec)
                          (hash-set! real-hash state-vec
                                     (cons prog (hash-ref real-hash state-vec)))
                          (hash-set! real-hash state-vec (list prog))))
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
	    (when (debug-inst my-inst)
                   (pretty-display `(inner ,level ,(length inters-fw)))
                  )

            (for ([inter inters-fw])
              (let ([t0 (current-milliseconds)]
		    [out-vec #f])

		(if (and (> level 0) (hash-has-key? cache-level inter))
		    (set! out-vec (hash-ref cache-level inter))
		    (let* ([s1 (current-milliseconds)]
                           [pass (prescreen my-inst inter)]
                           [out
                            (and pass
                            (with-handlers*
                             ([exn? (lambda (e) #f)])
                             (send simulator-abst interpret (vector my-inst)
                                   (send machine vector->progstate inter)
                                   ce-out-level)))]
                           [s2 (current-milliseconds)]
                           )
                      (when (debug-inst my-inst)
                            (pretty-display `(inter ,level ,inter))
                            (pretty-display `(out ,out)))
                            
		      (set! out-vec (and out (mask-in (send machine progstate->vector out) my-live2)))
		      ;;(set! out-vec (and out (send machine progstate->vector out)))
                      (set! t-interpret-0 (+ t-interpret-0 (- s2 s1)))
                      (set! c-interpret-0 (add1 c-interpret-0))
		      (hash-set! cache-level inter out-vec)))

		(let ([t1 (current-milliseconds)])
		  (set! t-interpret (+ t-interpret (- t1 t0)))
		  (set! c-interpret (add1 c-interpret))
                  )

                (when (debug-inst my-inst)
                      (pretty-display `(my-live2 ,my-live2))
                      ;;(pretty-display `(out-vec ,out-vec))
                      )

		(when 
		 out-vec
		 (let ([prune (send enum get-pruning-info out-vec)]
                       ;; TODO use out instead of out-vec?
                       [s0 (current-milliseconds)])

                   (when (debug-inst my-inst)
                         (pretty-display `(prune ,prune ,(hash-has-key? my-classes-bw-level prune))))
		   (when
		    (hash-has-key? my-classes-bw-level prune)
		    (let* ([pairs (hash->list (hash-ref my-classes-bw-level prune))]
			   [s1 (current-milliseconds)])
		      (for ([pair pairs])
			   (let* ([t0 (current-milliseconds)]
				  [live-mask (car pair)]
				  [classes (cdr pair)]
                                  [_ (when (debug-inst my-inst)
                                           (pretty-display `(live-mask ,level ,live-mask))
                                           (pretty-display `(KEYS ,level ,(hash-keys classes)))
                                           )]
				  [out-vec-masked 
				   (if (or (and try-cmp (not (equal? live-mask my-live2)))
                                           (not my-live2))
				       (mask-in out-vec live-mask)
				       out-vec)]
				  [t1 (current-milliseconds)]
				  [has-key (and out-vec-masked
                                                (hash-has-key? classes out-vec-masked))]
                                  [_ (when (debug-inst my-inst)
                                           (pretty-display `(out-vec-maked ,level ,out-vec-masked))
                                           (pretty-display `(has-key ,level ,has-key)))]
				  [progs-set (and has-key (hash-ref classes out-vec-masked))]
				  [t2 (current-milliseconds)]
				  [new-candidates
				   (and progs-set
					(if (= level 0)
					    (set->list progs-set)
					    (intersect candidates progs-set)))]
				  [t3 (current-milliseconds)])
			     (set! t-mask (+ t-mask (- t1 t0)))
			     (set! t-hash (+ t-hash (- t2 t1)))
			     (set! t-intersect (+ t-intersect (- t3 t2)))
                             (set! c-mask (add1 c-mask))
			     (when
			      (and new-candidates (not (empty? new-candidates)))
                              (when (debug-inst my-inst)
                                    (pretty-display `(pass!!! ,level ,(= 1 (- ce-count level)))))
			      (if (= 1 (- ce-count level))
				  (begin
                                    (when (debug-inst my-inst)
                                          (pretty-display `(check-eqv-leaf ,level ,ce-count)))
				    (check-eqv (hash-ref real-hash inter)
					       (map id->real-progs new-candidates)
					       my-inst ce-count)
				    (set! ce-count ce-count-extra)
				    )
				  (let (
                                        [_ (when (debug-inst my-inst)
                                                 (pretty-display `(call-outer ,(add1 level))))]
                                        [a (outer (hash-ref real-hash inter)
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


      (define (build-hash my-hash iterator) 
        ;; Call instruction generator
        (define inst-liveout-vreg (iterator))
        (define my-inst (first inst-liveout-vreg))
	(define my-liveout (second inst-liveout-vreg))

        (define cache (make-hash))
        (when 
         my-inst
         (when debug
               (send printer print-syntax-inst (send printer decode-inst my-inst))
               ;;(pretty-display my-liveout)
               )

         (define (recurse x states2-vec level)
           (define ce-out-level (vector-ref ce-out level))
           (if (list? x)
               (class-insert! classes my-liveout (reverse states2-vec) (concat x my-inst))
               (for ([pair (hash->list x)])
                    (let* ([state-vec (car pair)]
                           [val (cdr pair)]
                           [out 
                            (if (and (list? val) (hash-has-key? cache state-vec))
                                (hash-ref cache state-vec)
                                (let ([tmp
                                       (and
                                        (prescreen my-inst state-vec)
                                       (with-handlers*
                                        ([exn? (lambda (e) #f)])
                                        (send machine progstate->vector 
                                              (send simulator-abst interpret 
                                                    (vector my-inst)
                                                    (send machine vector->progstate state-vec)
                                                    ce-out-level))))])
                                  (when (list? val) (hash-set! cache state-vec tmp))
                                  tmp))
                            ])
                      (when out (recurse val (cons out states2-vec) (add1 level)))))))
         
         (recurse my-hash (list) 0)
         (build-hash my-hash iterator)
	 ))

      (define (build-hash-bw-all test)
        (define my-ce-out-vec (vector-ref ce-out-vec test))
        (define same #f)
        (for ([i test] #:break same)
             (when (equal? my-ce-out-vec (vector-ref ce-out-vec i))
                   (set! same i)))
	(pretty-display `(build-hash-bw-all ,test ,same))
        (when (= (vector-ref classes-bw-expand 0) test)
              (vector-set! classes-bw-expand 0 (add1 test))
              (class-init-bw! (vector-ref classes-bw 0)
                              live2-list test my-ce-out-vec)
              )
	(for ([step step-bw])
             (when
              (= (vector-ref classes-bw-expand (add1 step)) test)
              (vector-set! classes-bw-expand (add1 step) (add1 test))
              (if same
                  (let ([current (vector-ref classes-bw (add1 step))])
                    (for ([pair (hash->list current)])
                         (let ([live-list (car pair)]
                               [my-hash (cdr pair)])
                           (vector-set! my-hash test (vector-ref my-hash same)))))
                
                  (let ([prev (vector-ref classes-bw step)]
                        [current (vector-ref classes-bw (add1 step))])
                    (set! c-behaviors-bw 0)
                    (set! c-progs-bw 0)
                    (for ([pair (hash->list prev)])
                         (let* ([live-list (car pair)]
                                [my-hash (cdr pair)]
                                [rep (findf (lambda (x) x) (vector->list my-hash))])
                           (when rep
                                 (define prune (hash-keys rep))
                                 (define iterator (send enum generate-inst 
                                                        #f live-list #f prune
                                                        #:try-cmp try-cmp
                                                        #:step-fw (add1 step-fw)
                                                        #:step-bw (sub1 step-bw)))
                                 (build-hash-bw test current live-list my-hash iterator)
                                 )))
                    (pretty-display `(behavior-bw ,step ,test ,c-behaviors-bw ,c-progs-bw ,(- (current-seconds) start-time)))))))
        )

      (define (build-hash-bw test current old-liveout my-hash iterator)
        (define my-ce-out (vector-ref ce-out test))
	(define my-hash-test (vector-ref my-hash test))
	(define (inner)
	  (define inst-liveout-vreg (iterator))
	  (define my-inst (first inst-liveout-vreg))
	  (define my-liveout (third inst-liveout-vreg))

	  (when my-inst
                (when debug
                      (send printer print-syntax-inst (send printer decode-inst my-inst))
                      )
                (define inst-id (inst->id my-inst))
		(for* ([live2states (hash-values my-hash-test)]
                       [mapping (hash-values live2states)]
		       [pair (hash->list mapping)])
		      (let* ([out-vec (car pair)]
			     [progs (cdr pair)]
			     [in-list
                              (send inverse interpret-inst my-inst
                                    (send machine vector->progstate out-vec)
                                    my-ce-out)]
                             )
			(when (and in-list (not (empty? in-list)))
                              (let ([new-progs (concat-progs inst-id progs)])
                                (for ([in in-list])
                                     (class-insert-bw! current my-liveout test 
                                                       (send machine progstate->vector in)
                                                       new-progs))))
                        )
                      )
		(inner)
		))
	(and my-hash-test (inner)))

      (define middle 0)
      (define (refine-all hash1 live1 hash2 live2 iterator)
        (when (> (- (current-seconds) start-time) time-limit)
              (yield "timeout"))
	(define inst-liveout-vreg (iterator))
        (define my-inst (first inst-liveout-vreg))
        (when 
         my-inst
         (when
          verbo
          (send printer print-syntax-inst (send printer decode-inst my-inst)))
         (set! middle (add1 middle))
         (define ttt (current-milliseconds))
         (refine hash1 hash2 my-inst live1 live2)
         (when 
          (and verbo (> (- (current-milliseconds) ttt) 500))
          (pretty-display (format "search ~a ~a = ~a + ~a + ~a | ~a\t(~a + ~a/~a)\t~a/~a ~a ~a/~a\t[~a/~a]\t~a/~a\t~a/~a (~a) ~a/~a ~a/~a " 
                                  (- (current-milliseconds) ttt) ce-count-extra
                                  t-refine t-collect t-check
                                  t-build t-build-inter t-build-hash c-build-hash
                                  t-mask c-mask t-hash t-intersect c-intersect
                                  t-interpret-0 c-interpret-0
                                  t-interpret c-interpret
                                  t-extra c-extra c-check
                                  t-verify c-verify
                                  t-ce-abst c-ce-abst
                                  )))
         (set! t-build 0) (set! t-build-inter 0) (set! t-build-hash 0) (set! t-mask 0) (set! t-hash 0) (set! t-intersect 0) (set! t-interpret-0 0) (set! t-interpret 0) (set! t-extra 0) (set! t-verify 0) (set! t-ce-abst 0)
         (set! c-build-hash 0)  (set! c-mask 0)(set! c-intersect 0) (set! c-interpret-0 0) (set! c-interpret 0) (set! c-extra 0) (set! c-check 0) (set! c-verify 0) (set! c-ce-abst 0)
         (set! t-refine 0) (set! t-collect 0) (set! t-check 0)
         (refine-all hash1 live1 hash2 live2 iterator)
         ))


      (define (main-loop size)
        (when
         (>= size size-from)
         (pretty-display (format "\nSIZE = ~a" size))
         
         (define keys (hash-keys classes))
         (define keys-bw (hash-keys (vector-ref classes-bw step-bw)))
         (set! keys (sort-live keys))
         (set! keys-bw (sort-live-bw keys-bw))
         (when #t
               (for ([key keys])
                    (pretty-display `(key ,(entry-live key) ,(entry-prune key))))
               (pretty-display `(keys-bw ,step-bw ,keys-bw)))
         
         (define order 0)
         ;; Search
         (define ttt (current-milliseconds))
         (for* ([key1 keys]
                [live2 keys-bw])
               (let* ([my-hash2 (hash-ref (vector-ref classes-bw step-bw) live2)]
                      [pass (for/and ([i ntests]) (vector-ref my-hash2 i))])
                 (when
                  pass
                  (let* ([prune2 (hash-keys (vector-ref my-hash2 0))]
                         [prune1 (entry-prune key1)]
                         [live1 (entry-live key1)]
                         [my-hash1 (hash-ref classes key1)]
                         [iterator
                          (send enum generate-inst 
                                live1 live2 prune1 prune2
                                #:try-cmp try-cmp
                                #:step-fw step-fw
                                #:step-bw step-bw)])
                    (pretty-display `(refine ,order ,live1 ,prune1 ,live2 ,prune2 ,(- (current-seconds) start-time)))
                    (refine-all my-hash1 live1 my-hash2 live2 iterator)
                    (pretty-display `(middle-count ,middle))
                    (set! order (add1 order))
                    )))))
        
        (when (and (< size size-to) (or (not cost) (> cost (add1 size))))
              (cond
               [(and (< step-bw step-bw-max) (> step-fw (* 2 step-bw)))
                (set! step-bw (add1 step-bw))
                (newline)
                (pretty-display (format "GROW-BW: ~a" step-bw))
                ;; Grow backward
                (for ([test ntests]) (build-hash-bw-all test))
                ]

               [else
                (set! step-fw (add1 step-fw))

                ;; Grow forward
                (newline)
                (pretty-display (format "GROW-FW: ~a" step-fw))
                (set! c-behaviors 0)
                (set! c-progs 0)
                (set! classes (make-hash))
                (for ([pair (hash->list prev-classes)])
                     (when (> (- (current-seconds) start-time) time-limit)
                           (yield "timeout"))
                     (let* ([key (car pair)]
                            [live-list (entry-live key)]
                            [prune (entry-prune key)]
                            [my-hash (cdr pair)]
                            [iterator (send enum generate-inst 
                                            live-list #f prune #f
                                            #:try-cmp try-cmp
                                            #:step-fw (sub1 step-fw)
                                            #:step-bw (add1 step-bw))])
                       (pretty-display `(live ,live-list ,prune ,(- (current-seconds) start-time)))
                       (build-hash my-hash iterator)))
                (set! prev-classes (copy classes))
                (pretty-display `(behavior ,c-behaviors ,c-progs ,(- (current-seconds) start-time)))
                ])
              
              (main-loop (add1 size))
              )
        )
      
      (main-loop 1)
      (yield #f)
      )
    ))
