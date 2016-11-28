#lang racket

(require "inst.rkt" "stat.rkt" "machine.rkt")

(provide stochastic%)

(define stochastic%
  (class object%
    (super-new)
    (public superoptimize inst-copy-with-op inst-copy-with-args
            get-mutations mutate 
            mutate-opcode mutate-operand mutate-swap
            mutate-operand-specific mutate-other
            random-instruction 
	    random-args-from-op pop-count32 pop-count64 correctness-cost-base correctness-one-many)
    (abstract correctness-cost)
              
;;;;;;;;;;;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;;;
    (init-field machine printer validator simulator syn-mode
                [parser #f]
                [stat (new stat% [printer printer])]
                [input-file #f]
                [w-error 9999]
                [beta 1]
                [nop-mass 0.8]
                [ntests 16]
                [mutate-dist 
                 #hash((opcode . 1) (operand . 1) (swap . 1) (instruction . 1))]
                [live-in #f])
    
    (define nop-id (get-field nop-id machine))
  
    ;; (define (print-mutation-info)
    ;;   (for ([op opcodes]
    ;;         [id (in-naturals)])
    ;;        (pretty-display `(opcode ,op ,(get-mutations id))))
    ;;   )
  
;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;
    (define (inst-copy-with-op x op) (struct-copy inst x [op op]))
    (define (inst-copy-with-args x args) (struct-copy inst x [args args]))

    (define (superoptimize spec constraint 
                           name time-limit size 
                           #:prefix [prefix (vector)]
                           #:postfix [postfix (vector)]
                           #:assume [assumption (send machine no-assumption)]
                           #:input-file [input-file #f]
                           #:start-prog [start #f])
      (send machine reset-opcode-pool)
      (send machine reset-arg-ranges)
      (send validator adjust-memory-config spec)

      ;; 1) User-provided live-in
      ;; (define live-in-user (send machine progstate->vector this-live-in))
      ;; (for ([x prefix])
      ;;      (set! live-in-user (send machine update-live live-in x)))

      ;; 2) Combined automatic live-in
      ;; (define live1 (send validator get-live-in (vector-append spec postfix) constraint))
      ;; (define live0 (send validator-abst get-live-in (vector-append prefix spec postfix) constraint))
      ;; (define live0-list (send machine progstate->vector live0))

      ;; (define live1-list-alt live0-list)
      ;; (for ([x prefix])
      ;;      (set! live1-list-alt (send machine update-live live1-list-alt x)))
      ;; (define live1-list (send machine progstate->vector live1))
      ;; (set! live-in (combine-live live1-list-alt live1-list))

      ;; 3) get-live-in then update-live
      (define live0 (send validator get-live-in (vector-append prefix spec postfix) constraint))
      (define live0-list (send machine progstate->vector live0))

      (set! live-in live0-list)
      (for ([x prefix])
           (set! live-in (send machine update-live live-in x)))

      ;; (unless (vector? live-in-user)
      ;;         (for ([l1 live-in]
      ;;               [l2 live-in-user])
      ;;              (cond
      ;;               [(boolean? l1)
      ;;                (when (and l2 (not l1))
      ;;                      (pretty-display `(live-in ,live-in ,live-in-user))
      ;;                      (raise "done"))]
      ;;               [else
      ;;                (for ([ll1 l1]
      ;;                      [ll2 l2])
      ;;                     (when (and ll2 (not ll1))
      ;;                           (pretty-display `(live-in ,live-in ,live-in-user))
      ;;                           (raise "done")))])))
                          

      
      (send machine analyze-args (vector) spec (vector) live-in constraint)
      (send machine analyze-opcode (vector) spec (vector))
      ;; Generate testcases
      (when debug
            (pretty-display ">>> Phase 0: print mutation info")
            ;;(print-mutation-info)
            (pretty-display ">>> Phase 1: generate input states")
            (if input-file
                (pretty-display ">>> Inputs from file")
                (pretty-display ">>> Auto generate"))
            )
      (define inits 
        (if input-file
            (map cdr (send machine get-states-from-file input-file))
            (send validator generate-input-states ntests (vector-append prefix spec postfix) assumption)))

      (define inputs (map (lambda (x) (send simulator interpret prefix x)) inits))
        
      (when debug
            (for ([i inputs])
                 (send machine display-state i))
            (pretty-display ">>> Phase 2: generate output states"))
      (define outputs (map (lambda (x) (send simulator interpret spec x)) inputs))
      (when debug
            (for ([i outputs])
                 (send machine display-state i))
            )
      (set-field! best-correct-program stat spec)
      (set-field! best-correct-cost stat (send simulator performance-cost spec))
      (send stat set-name name)
      (pretty-display (format ">>> Finish generating inputs outputs at ~a s."
                              (- (current-seconds) (get-field start-time stat))))

      ;; MCMC sampling
      (define-syntax-rule (get-sketch) 
        (random-insts (if size size (vector-length spec))))
      (mcmc-main prefix postfix spec 
                 (cond
                  [start start]
                  [syn-mode (get-sketch)]
                  [else spec])
                 inputs outputs 
		 (send validator get-live-in postfix constraint)
		 assumption time-limit))
          
    (define (random-insts n)
      (when debug 
            (pretty-display (format "Create ~a random instructions" n)))
      (define my-live-in live-in)
      (for/vector ([i n]) 
        (let ([x (random-instruction i n my-live-in)])
          (when debug (pretty-display (format "inst #~a: ~a" i
                                              (send machine get-opcode-name (inst-op x)))))
          (set! my-live-in (send machine update-live my-live-in x))
          x)))
      
    ;; Mutate by swapping instructions at 'index' with another instruction
    ;; index: index to be swapped
    ;; entry: instruction at index in p
    ;; p: entire program
    (define (mutate-swap index entry p)
      (define new-p (vector-copy p))
      (define index2 (random-from-list-ex (range (vector-length p)) index))
      
      ;; (define n (vector-length p))
      ;; (define index-min (min index index2))
      ;; (define index-max (max index index2))
      ;; (define my-live-in live-in)
      ;; (for ([i index-min])
      ;;      (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
      ;; (define valid-opcode-pool (send machine get-valid-opcode-pool index-min n my-live-in))

      (when debug
            (pretty-display " >> mutate swap")
            (pretty-display (format " --> swap = ~a" index2))
            ;; (pretty-display (format " --> valid = ~a in ~a"
            ;;                         (send machine get-opcode-name (inst-op (vector-ref p index-max)))
            ;;                         (map (lambda (x) (send machine get-opcode-name x)) valid-opcode-pool)))
            )
      (cond
       [#t ;;(member (inst-op (vector-ref p index-max)) valid-opcode-pool)
        (vector-set! new-p index (vector-ref new-p index2))
        (vector-set! new-p index2 entry)
        (send stat inc-propose `swap)
        new-p]
       [else (mutate p)]))

    ;; Mutate opcode.
    ;; index: index to be mutated
    ;; entry: instruction at index in p
    ;; p: entire program
    (define (mutate-opcode index entry p)
      ;; (define n (vector-length p))
      ;; (define my-live-in live-in)
      ;; (for ([i index])
      ;;      (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
      ;; (define valid-opcode-pool (send machine get-valid-opcode-pool index n my-live-in))
      
      (define opcode-id (inst-op entry))
      (define opcode-name (send machine get-opcode-name opcode-id))
      (define class (send machine get-class-opcodes opcode-id))
      ;; (define class (filter
      ;;                (lambda (x) (member x valid-opcode-pool))
      ;;                (send machine get-class-opcodes opcode-id)))
      (when debug
            (pretty-display (format " >> mutate opcode"))
            (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
            (pretty-display (format " --> class = ~a" class)))
      (cond
       [class
        (define new-opcode-id (random-from-list-ex class opcode-id))
        (define new-p (vector-copy p))
        (when debug
              (pretty-display (format " --> new = ~a ~a" (send machine get-opcode-name new-opcode-id) new-opcode-id)))
        (vector-set! new-p index (inst-copy-with-op entry new-opcode-id))
        (send stat inc-propose `opcode)
        new-p]

       [else (mutate p)]))

    ;; Mutate operand.
    (define (mutate-operand index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (send machine get-opcode-name opcode-id))
      (define args (vector-copy (inst-args entry)))
      (define my-live-in live-in)
      (for ([i index])
           (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
      (define ranges (send machine get-arg-ranges opcode-id entry my-live-in))
      (cond
       [(and ranges (> (vector-length ranges) 0))
        (define args (vector-copy (inst-args entry)))
        (define okay-indexes (list))
        (for ([range ranges]
              [i (vector-length ranges)])
             (when (or (equal? range #f) (> (vector-length range) 1))
                   (set! okay-indexes (cons i okay-indexes))))
        
        (cond
         [(empty? okay-indexes) (mutate p)]
         [else
          (define change (random-from-list okay-indexes))
          (define valid-vals (vector-ref ranges change))
          (define new-val 
            (if (vector? valid-vals)
                (random-from-vec-ex valid-vals (vector-ref args change))
                (mutate-operand-specific opcode-name args change live-in)))
          
          (define new-p (vector-copy p))
          (when debug
                (pretty-display (format " --> org = ~a ~a" opcode-name args))
                (pretty-display (format " --> choices = ~a" (vector-ref ranges change)))
                (pretty-display (format " --> new = [~a]->~a" change new-val)))
          (vector-set! args change new-val)
          (vector-set! new-p index (inst-copy-with-args entry args))
          (send stat inc-propose `operand)
          new-p])
        ]

       [else (mutate p)]))

    ;; Mutate an entire instruction.
    (define (mutate-instruction index entry p)
      (define new-p (vector-copy p))
      (define opcode-id (inst-op entry))
      (define opcode-name (send machine get-opcode-name opcode-id))
      (define n (vector-length p))
      (define my-live-in live-in)
      (for ([i index])
           (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
      
      (define new-opcode-id
        (cond
         [(and (not (equal? opcode-id nop-id)) (< (random) nop-mass))
          (send stat inc-propose `nop)
          nop-id]

         [else
          (define valid-opcodes (send machine get-valid-opcode-pool index n my-live-in))
          (when (empty? valid-opcodes)
                (when debug (pretty-display " --> choices = (reset)"))
                (set! valid-opcodes (get-field opcode-pool machine)))
          (when debug
                (pretty-display
                 (format " --> choices = ~a"
                         (map (lambda (x) (send machine get-opcode-name x)) valid-opcodes))))
          (send stat inc-propose `inst)
          (random-from-list valid-opcodes)]))
      
      (when debug
            (pretty-display (format " >> mutate instruction ~a" (send machine get-opcode-name new-opcode-id))))
      (define new-entry (random-instruction index n my-live-in new-opcode-id))
      (vector-set! new-p index new-entry)
      new-p)
    
    ;; Create a new instruction with operands that are live (in live-in) and with opcode-id if specified.
    ;; live-in: vector/list/pair format
    (define (random-instruction
             index n live-in
             [opcode-id (random-from-list (send machine get-valid-opcode-pool index n live-in))])
      (when #f
            (pretty-display `(pool ,(send machine get-valid-opcode-pool index n live-in))))
      (when debug (pretty-display `(random-instruction ,opcode-id)))
      (define args (random-args-from-op opcode-id live-in))
      (if args
          (inst opcode-id args)
          (random-instruction index n live-in)))
    
    ;; Create random operands from opcode.
    (define (random-args-from-op opcode-id live-in)
      (define ranges (send machine get-arg-ranges opcode-id #f live-in))
      (when debug (pretty-display (format " --> ranges ~a" ranges)))
      (define pass (and ranges (for/and ([range ranges]) (> (vector-length range) 0))))
      (and pass
           (for/vector ([i (vector-length ranges)])
                       (random-from-vec (vector-ref ranges i)))))
    
    (define (mutate-operand-specific opcode-name args index)
      (raise "mutate-operand-specific: unimplemented"))

    (define (mutate-other index entry p stat type)
      (raise "mutate-other: unimplemented"))

    (define (get-mutations opcode-id)
      (define arg-types (send machine get-arg-types opcode-id))
      (define mutations '(instruction swap))
      (when (> (vector-length arg-types) 0)
            (set! mutations (cons 'operand mutations)))
      (when (> (length (send machine get-class-opcodes opcode-id)) 1)
            (set! mutations (cons 'opcode mutations)))

      mutations)

    ;; Get a list of valid mutations given an opcode.
    (define (get-mutate-type opcode-id)
      (define mutations (get-mutations opcode-id))
      
      (when debug (pretty-display `(mutations ,opcode-id ,mutations)))
      (define sum 0)
      (define prop (map (lambda (x) 
                          (let ([v (hash-ref mutate-dist x)])
                            (set! sum (+ sum v))
                            sum))
                        mutations))
      (set! prop (map (lambda (x) (exact->inexact (/ x sum))) prop))
      (define rand (random))
      (define (loop name-list prop-list)
        (if (<= rand (car prop-list))
            (car name-list)
            (loop (cdr name-list) (cdr prop-list))))
      (loop mutations prop))

    ;; Mutate program p.
    (define (mutate p)
      (define vec-len (vector-length p))
      (define index (random vec-len))
      (define entry (vector-ref p index))
      (define opcode-id (inst-op entry))
      (define opcode-name (send machine get-opcode-name opcode-id))
      (define type (get-mutate-type opcode-id))
      (when debug 
            (pretty-display 
             (format " >> mutate = ~a, index = ~a" type index)))

      (cond
       [(equal? type `opcode)      (mutate-opcode index entry p)]
       [(equal? type `operand)     (mutate-operand index entry p)]
       [(equal? type `instruction) (mutate-instruction index entry p)]
       [(equal? type `swap)        (mutate-swap index entry p)]
       [else                       (mutate-other index entry p type)]))
      
    ;; MCMC sampling process.
    (define (mcmc-main prefix postfix target init inputs outputs constraint assumption time-limit)
      (pretty-display ">>> start MCMC sampling")
      (pretty-display ">>> Phase 3: stochastic search")
      (pretty-display "start-program:")
      (send printer print-syntax (send printer decode init))
      ;; (pretty-display "constraint:")
      ;; (send machine display-state constraint)
      (define syn-mode #t)
    
      (define (reduce-one p vec-len)
        (define index (random vec-len))
        (define new-p 
          (vector-append (vector-copy p 0 index) (vector-copy p (add1 index) vec-len)))
        (define cost (or (car (cost-all-inputs new-p w-error)) w-error))
        (values new-p cost))

      (define (reduce-size p cost size [ps (list)] [costs (list)])
        (when debug (pretty-display `(reduce-size ,(vector-length p) ,size ,(length ps))))
        (define vec-len (vector-length p))
        (cond
         [(= vec-len size)
          (values p cost)]
         [(>= (length ps) 4)
          (define min-cost (first costs))
          (define min-p (first ps))
          (for ([p-i (cdr ps)]
                [c-i (cdr costs)])
               (when (< c-i min-cost)
                     (set! min-cost c-i)
                     (set! min-p p-i)))
          (reduce-size min-p min-cost size)]
         [else
          (define-values (new-p new-cost) (reduce-one p vec-len))
          (if (<= new-cost cost)
              (reduce-size new-p new-cost size)
              (reduce-size p cost size (cons new-p ps) (cons new-cost costs)))])
        )

      (define (cost-one-input program input output)
	(define t1 (current-milliseconds))
	(define program-out
	  (with-handlers* 
	   ([exn:break? (lambda (e) (raise e))]
	    [exn? (lambda (e) 
		    (when debug 
			  (pretty-display "Error!")
			  (pretty-display (exn-message e)))
		    #f)]
	    )
	   (send simulator interpret program input output)))
	(and program-out
	     (let ([t2 (current-milliseconds)]
		   [ret (correctness-cost output program-out constraint)]
		   [t3 (current-milliseconds)])
	       (send stat simulate (- t2 t1))
	       (send stat check (- t3 t2))
	       ret)))
      
      (define (cost-all-inputs program okay-cost)
        (define change-mode #f)

        (define (loop correct inputs outputs)
          (when debug (pretty-display `(correct ,correct)))
          (cond
           [(> correct okay-cost) #f]
           [(empty? inputs) correct]
           [else
            (let ([cost (cost-one-input program (car inputs) (car outputs))])
              (if cost 
                  (loop (+ correct cost) (cdr inputs) (cdr outputs))
                  w-error))]
           ))

        (define correct
          (and (send simulator is-valid? program)
               (loop 0 inputs outputs)))
        (when debug (pretty-display `(final-correct ,correct ,(length inputs))))

        (define ce #f)
        (when (and (number? correct) (= correct 0))
              (send stat inc-validate)
              (define t1 (current-milliseconds))
              ;; (pretty-display `(counterexample))
              ;; (send printer print-syntax (send printer decode program))
              (set! ce (send validator counterexample 
                             (vector-append prefix target postfix)
                             (vector-append prefix program postfix)
                             constraint #:assume assumption))
              (if ce 
                  (begin
                    (set! correct 1)
                    (set! ce (send simulator interpret prefix ce))
                    (set! inputs (cons ce inputs))
                    (set! outputs (cons (send simulator interpret target ce) 
                                        outputs))
                    (pretty-display (format "Add counterexample. Total = ~a." (length inputs)))
                    (send machine display-state ce)
		    (send printer print-syntax (send printer decode program))
                    (when (> (length inputs) 100) (raise "ce > 100"))
                    )
                  (begin
                    (send stat inc-correct)
                    (when syn-mode (set! change-mode #t) (set! syn-mode #f))
                    ;;(send stat inc-correct)
                    ))
              
              (define t2 (current-milliseconds))
              (send stat validate (- t2 t1))
              )
        
        (if (number? correct)
            (let ([total-cost 
                   (if syn-mode correct (+ (send simulator performance-cost program) correct))])
              (when debug (pretty-display `(total-cost ,total-cost)))
              (when (< total-cost (get-field best-cost stat))
                    (send stat update-best program total-cost)
                    )
              ;; (when (= correct 0)
              ;;       (pretty-display "FOUND! correct-program (may not be better)")
              ;;       (pretty-display "output:")
              ;;       (send printer print-syntax (send printer decode program)))
              (when (and (= correct 0) (= total-cost (get-field best-correct-cost stat)))
                    (send stat update-best-correct-program program))
              (when (and (= correct 0) (< total-cost (get-field best-correct-cost stat)))
                    (pretty-display "NEW! best-correct-program")
                    ;; (pretty-display "program-eq? --> true")
                    ;; (pretty-display "target:")
                    ;; (send printer print-struct target)
                    (pretty-display "output:")
                    (send printer print-syntax (send printer decode program))
                    (send stat update-best-correct 
			  (send machine clean-code program prefix) 
			  total-cost)
                    )
              (if (or (<= total-cost okay-cost) change-mode) 
                  ;; return (correctness-cost . correct)
                  (cons total-cost (= correct 0))
                  (cons #f #f)))
            (cons #f #f))
        )

      (define (accept-cost current-cost)
        (- current-cost (/ (log (random)) beta)))

      ;; Main loop
      (define (iter current current-cost)
        (when debug (pretty-display ">>> iter >>>"))
        (define update-size (send stat inc-iter current-cost))
        (when (and update-size (or (<= (+ update-size 5) (vector-length current))
				   (and (< update-size (vector-length current))
					(< (random) 0.05))))
              (pretty-display (format ">>> reduce size from ~a to ~a" 
                                      (vector-length current) update-size))
              (cond
               [syn-mode
                (define-values (new-p new-cost) 
                  (reduce-size current current-cost update-size))
                (set! current new-p)
                (set! current-cost new-cost)
                ;; (define tmp (send printer encode
                ;;                   (send parser ir-from-file
                ;;                         (format "~a/best.s" (get-field dir stat)))))
                ;; (send machine analyze-args (vector) tmp (vector))
                ]

               [else
                (pretty-display ">>> steal best program")
                (define-values (best-cost len time id) (send stat get-best-info-stat))
                (set! current
                      (send printer encode
                            (send parser ir-from-file 
                                  (format "~a/best.s" (get-field dir stat)))))
                (set! current-cost best-cost)
                ]
              ))
        (define t1 (current-milliseconds))
        (define proposal (mutate current))
        (define t2 (current-milliseconds))
        (send stat mutate (- t2 t1))
        (when debug
              (pretty-display (format "================ Current (syn=~a) =================" syn-mode))
              (send printer print-struct (send printer decode current))
              ;; (define cost (cost-all-inputs current (arithmetic-shift 1 32)))
              ;; (pretty-display (format "actual cost: ~a" cost))
              (pretty-display (format "================ Propose (syn=~a) =================" syn-mode))
              (send printer print-struct (send printer decode proposal))
              )
        (define n-inputs (length inputs))
        (define okay-cost (accept-cost current-cost))
        (when debug (pretty-display ">>> start simulate"))
        (define cost-correct (cost-all-inputs proposal okay-cost))
        (when debug (pretty-display ">>> finish simulate"))
        (define proposal-cost (car cost-correct))
        (when debug
              (pretty-display (format "current cost: ~a" current-cost))
              (pretty-display (format "okay cost: ~a" okay-cost))
              (pretty-display (format "proposal cost: ~a" proposal-cost)))

        (if proposal-cost
            (begin
              (when debug
                    (pretty-display "================ ACCEPT! =================")
                    (send printer print-struct proposal))
              (send stat inc-accept)
              (when (> proposal-cost current-cost) 
                    (send stat inc-accept-higher))
              ;; Adjust cost due to new counterexample
              (when (> (length inputs) n-inputs)
                    (when debug (display (format "Adjust proposal cost from ~a " proposal-cost)))
		    (define more-cost (cost-one-input proposal (car inputs) (car outputs)))
		    (if more-cost
			(set! proposal-cost (sub1 (+ proposal-cost more-cost)))
			(set! proposal-cost w-error))
                    (when debug (pretty-display (format "to ~a." proposal-cost)))
                    )
              (iter (if (cdr cost-correct) 
                        (send machine clean-code proposal prefix)
                        proposal) 
                    proposal-cost))
            (begin
              ;; Adjust cost due to new counterexample
              (when (> (length inputs) n-inputs)
                    (when debug (display (format "Adjust current cost from ~a " current-cost)))
		    (define more-cost (cost-one-input current (car inputs) (car outputs)))
		    (if more-cost
			(set! current-cost (+ current-cost more-cost))
			(set! current-cost w-error))
                    (when debug (pretty-display (format "to ~a." current-cost)))
                    )
              (iter current current-cost))
            ))

      (with-handlers 
       ([exn:break? (lambda (e) (send stat print-stat-to-file))])
       
       (timeout time-limit
                (iter init 
                      ;; cost-all-inputs can return #f if program is invalid
                      (or (car (cost-all-inputs init w-error))
                          w-error)
                      ))))

    ;; Population count for 32-bit number
    (define (pop-count32 a)
      (set! a (- a (bitwise-and (arithmetic-shift a -1) #x55555555)))
      ;;(pretty-display a)
      (set! a (+ (bitwise-and a #x33333333)
                 (bitwise-and (arithmetic-shift a -2) #x33333333)))
      ;;(pretty-display a)
      (set! a (bitwise-and (+ a (arithmetic-shift a -4)) #x0f0f0f0f))
      (set! a (+ a (arithmetic-shift a -8)))
      (set! a (+ a (arithmetic-shift a -16)))
      (bitwise-and a #x3f))

    ;; Population count for 64-bit number
    (define (pop-count64 a)
      (set! a (- a (bitwise-and (arithmetic-shift a -1) #x5555555555555555)))
      ;;(pretty-display a)
      (set! a (+ (bitwise-and a #x3333333333333333)
                 (bitwise-and (arithmetic-shift a -2) #x3333333333333333)))
      ;;(pretty-display a)
      (set! a (bitwise-and (+ a (arithmetic-shift a -4)) #x0f0f0f0f0f0f0f0f))
      (set! a (+ a (arithmetic-shift a -8)))
      (set! a (+ a (arithmetic-shift a -16)))
      (set! a (+ a (arithmetic-shift a -32)))
      (bitwise-and a #x7f))

    ;; Helper function to calculate correctness cost.
    ;; state1, state2, constraint are vectors of the same length,
    ;; delta is a function that calculate the cost between two numbers.
    ;; 
    ;; Compute correctness cost sum of all bit difference in live entires.
    ;; This method takes into account of misalignment.
    ;; For example, if r0 of state1 = r1 of state2, the cost will be quite low.
    (define (correctness-cost-base state1 state2 constraint delta)
      (define correctness 0)
      (define n (vector-length state1))
      (for ([i n]
            [v constraint])
           (when
            v
            (let* ([v1 (vector-ref state1 i)]
                   [best-cost (delta v1 (vector-ref state2 i))]
                   [best-j i])
              ;; test i against all values, and find the lowest cost of i against j
              (for ([j n])
                   (let* ([v2 (vector-ref state2 j)]
                          [this-cost (delta v1 v2)])
                     (when (< this-cost best-cost)
                           (set! best-cost this-cost)
                           (set! best-j j))))
              (set! correctness (+ correctness best-cost))
              ;; if i != j, add mis-aligned penalty = 1
              (unless (= best-j i) (set! correctness (add1 correctness))))))
      correctness)

    ;; Helper function for computing correctness cost of a specific entry.
    ;; live: Is this entry live? #t = live
    ;; expect: expected value of the entry
    ;; this-val: actual value of the entry
    ;; other-vals: actual values of other entries nearby
    ;;             (using one move instruction to get to the entry of interest)
    ;; delta: lambda function computing correctness cost of given two values.
    (define (correctness-one-many live expect this-val other-vals delta)
      (cond
       [live
        (define best-cost (delta expect this-val))
        (define penalty 1)
        (for ([v other-vals])
             (let ([cost (+ (delta expect v) penalty)])
               (when (< cost best-cost)
                     (set! best-cost cost))))
        best-cost]
       [else 0]))

    ))
