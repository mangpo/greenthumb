#lang racket

(require "ast.rkt" "stat.rkt" "machine.rkt")

(provide stochastic%)

(define stochastic%
  (class object%
    (super-new)
    (public superoptimize inst-copy-with-op inst-copy-with-args
            get-mutations mutate 
            mutate-opcode mutate-operand
            mutate-operand-specific mutate-other
            random-instruction print-mutation-info
	    random-args-from-op adjust)
    (abstract correctness-cost)
              
;;;;;;;;;;;;;;;;;;;;; Parameters ;;;;;;;;;;;;;;;;;;;
    (init-field machine printer syn-mode
                [parser #f]
                [validator #f]
                [simulator #f]
                [base-cost #f]
                [stat (new stat% [printer printer])]
                [input-file #f]
                [w-error 9999]
                [beta 1]
                [nop-mass 0.8]
                [ntests 16]
                [mutate-dist 
                 #hash((opcode . 1) (operand . 1) (swap . 1) (instruction . 1))]
                [live-in #f])
    
    (define nop-id (send machine get-inst-id `nop))
    (define inst-id (get-field inst-id machine))
    (define classes (get-field classes machine))
  
    (define (print-mutation-info)
      (for ([op inst-id])
           (pretty-display `(opcode ,op ,(get-mutations op)))))
  
;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;
    (define (inst-copy-with-op x op) (struct-copy inst x [op op]))
    (define (inst-copy-with-args x args) (struct-copy inst x [args args]))

    (define (superoptimize spec constraint this-live-in
                           name time-limit size [extra-info #f]
                           #:prefix [prefix (vector)]
                           #:postfix [postfix (vector)]
                           #:assume [assumption (send machine no-assumption)]
                           #:input-file [input-file #f]
                           #:start-prog [start #f])
      (send machine analyze-args (vector) spec (vector))
      (send machine analyze-opcode (vector) spec (vector))
      (define my-live-in
        (send validator get-live-in (vector-append spec postfix)
              constraint extra-info))
      (set! live-in (send machine get-operand-live this-live-in))
      (pretty-display `(live-in ,live-in))
      (pretty-display (format "Base-cost: ~a" base-cost))
      ;; Generate testcases
      (when debug 
            (pretty-display ">>> Phase 0: print mutation info")
            (print-mutation-info)
            ;;(raise "done")
            (pretty-display ">>> Phase 1: generate input states")
            (if input-file
                (pretty-display ">>> Inputs from file")
                (pretty-display ">>> Auto generate"))
            )
      (define inits 
        (if input-file
            (map cdr (send machine get-states-from-file input-file))
            (send validator generate-input-states ntests (vector-append prefix spec postfix)
                  assumption extra-info)))

      (define inputs (map (lambda (x) (send simulator interpret prefix x #:dep #f)) inits))
        
      (when debug
            (for ([i inputs])
                 (send machine display-state i))
            (pretty-display ">>> Phase 2: generate output states"))
      (define outputs (map (lambda (x) (send simulator interpret spec x #:dep #t)) inputs))
      (when debug
            (for ([i outputs])
                 (send machine display-state i))
            )
      (set-field! best-correct-program stat spec)
      (set-field! best-correct-cost stat (add1 (send simulator performance-cost spec))) ;; TODO: remove add1 for real tool
      (send stat set-name name)

      ;; MCMC sampling
      (define-syntax-rule (get-sketch) 
        (random-insts (if size size (vector-length spec))))
      (mcmc-main prefix postfix spec 
                 (cond
                  [start start]
                  [syn-mode (get-sketch)]
                  [else spec])
                 inputs outputs 
		 (send validator get-live-in postfix constraint extra-info)
		 assumption time-limit extra-info))
          
    (define (random-insts n)
      (when debug 
            (pretty-display (format "Create ~a random instructions" n)))
      (define my-live-in live-in)
      (for/vector ([i n]) 
        (let ([x (random-instruction my-live-in)])
          (when debug (pretty-display (format "inst #~a: ~a" i (inst-op x))))
          (set! my-live-in (send machine update-live my-live-in x)) ;; TODO
          x)))
      

    ;; Generic across architectures
    (define (mutate-swap index entry p)
      (define new-p (vector-copy p))
      (define index2 (random-from-list-ex (range (vector-length p)) index))
      
      (define index-small (min index index2))
      (define index-large (max index index2))
      (define entry-large (vector-ref p index-large))
      (define my-live-in live-in)
      (for ([i index-small])
           (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
      (define opcode-id (inst-op entry-large))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define ranges (send machine get-arg-ranges opcode-name entry-large my-live-in))
      (define pass
        (for/and ([range ranges]
                  [arg (inst-args entry-large)])
                 (vector-member arg range)))
      (when debug
            (pretty-display " >> mutate swap")
            (pretty-display (format " --> swap = ~a" index2)))

      (cond
       [pass
        (when debug (pretty-display " --> pass"))
        (vector-set! new-p index (vector-ref new-p index2))
        (vector-set! new-p index2 entry)
        (send stat inc-propose `swap)
        new-p]
      
       [else
        (when debug (pretty-display " --> fail"))
        (mutate p)]))

    ;; Generic across architectures
    (define (mutate-opcode index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define class-id (send machine get-class-id opcode-name))
      (define class (and class-id (vector-ref (get-field classes-filtered machine) class-id)))
      (when debug
            (pretty-display (format " >> mutate opcode"))
            (pretty-display (format " --> org = ~a ~a" opcode-name opcode-id))
            (pretty-display (format " --> class = ~a" class)))
      (cond
       [class
        (define new-opcode-id (random-from-list-ex class opcode-id))
        (define new-p (vector-copy p))
        (when debug
              (pretty-display (format " --> new = ~a ~a" (send machine get-inst-name new-opcode-id) new-opcode-id)))
        (vector-set! new-p index (inst-copy-with-op entry new-opcode-id))
        (send stat inc-propose `opcode)
        new-p]

       [else (mutate p)]))

    (define (mutate-operand index entry p)
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define args (vector-copy (inst-args entry)))
      (define my-live-in live-in)
      (for ([i index])
           (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
      (define ranges (send machine get-arg-ranges opcode-name entry my-live-in))
      (cond
       [(> (vector-length ranges) 0)
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
                (pretty-display (format " --> new = [~a]->~a)" change new-val)))
          (vector-set! args change new-val)
          (vector-set! new-p index (inst-copy-with-args entry args))
          (send stat inc-propose `operand)
          new-p])
        ]

       [else (mutate p)]))

    (define (mutate-instruction index entry p)
      (define new-p (vector-copy p))
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define new-opcode-id
        (if (and (not (equal? opcode-id nop-id)) (< (random) nop-mass))
            (begin (send stat inc-propose `nop) 
                   nop-id)
            (begin (send stat inc-propose `inst) 
		   (random-from-list (get-field inst-pool machine)))))
      (when debug
            (pretty-display (format " >> mutate instruction ~a" (vector-ref inst-id new-opcode-id))))
      (define my-live-in live-in)
      (for ([i index])
           (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
      (define new-entry (random-instruction my-live-in new-opcode-id))
      (vector-set! new-p index new-entry)
      new-p)
    
    (define (random-instruction live-in [opcode-id (random-from-list (get-field inst-pool machine))])
      (when debug (pretty-display `(random-instruction ,opcode-id)))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define args (random-args-from-op opcode-name live-in))
      (if args
          (inst opcode-id args)
          (random-instruction live-in)))
    
    (define (random-args-from-op opcode-name live-in)
      (define ranges (send machine get-arg-ranges opcode-name #f live-in))
      (when debug (pretty-display (format " --> ranges ~a" ranges)))
      (define pass (for/and ([range ranges]) (> (vector-length range) 0)))
      (and pass
           (for/vector ([i (vector-length ranges)])
                       (random-from-vec (vector-ref ranges i)))))
    
    (define (mutate-operand-specific opcode-name args index)
      (raise "mutate-operand-specific: unimplemented"))

    (define (mutate-other index entry p stat type)
      (raise "mutate-other: unimplemented"))

    (define (get-mutations opcode-name)
      (define mutations '(instruction swap))
      (unless (equal? opcode-name `nop)
              (set! mutations (cons `opcode mutations))
              (set! mutations (cons `operand mutations)))
      mutations)

    (define (get-mutate-type opcode-name)
      (define mutations (get-mutations opcode-name))
      
      ;; TODO: redundant
      (when debug (pretty-display `(mutations ,mutations)))
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

    (define (mutate p)
      (define vec-len (vector-length p))
      (define index (random vec-len))
      (define entry (vector-ref p index))
      (define opcode-id (inst-op entry))
      (define opcode-name (vector-ref inst-id opcode-id))
      (define type (get-mutate-type opcode-name))
      (when debug 
            (pretty-display 
             (format " >> mutate = ~a, index = ~a" type index)))

      (cond
       [(equal? type `opcode)      (mutate-opcode index entry p)]
       [(equal? type `operand)     (mutate-operand index entry p)]
       [(equal? type `instruction) (mutate-instruction index entry p)]
       [(equal? type `swap)        (mutate-swap index entry p)]
       [else                       (mutate-other index entry p type)]))
      
    

    (define (mcmc-main prefix postfix target init inputs outputs constraint assumption time-limit extra-info)
      (pretty-display ">>> start MCMC sampling")
      (pretty-display ">>> Phase 3: stochastic search")
      (pretty-display "start-program:")
      (send printer print-struct init)
      ;; (pretty-display "constraint:")
      ;; (send machine display-state constraint)
      (define syn-mode #t)

    ;; (define (fix-program p)
    ;;   (define my-live-in live-in)
    ;;   (define (keep-or-new entry)
    ;;     (define opcode-id (inst-op entry))
    ;;     (define opcode-name (vector-ref inst-id opcode-id))
    ;;     (define ranges (send machine get-arg-ranges opcode-name entry my-live-in))
    ;;     (define pass
    ;;       (for/and ([range ranges]
    ;;                 [arg (inst-args entry)])
    ;;                (vector-member arg range)))

    ;;     (define new-entry
    ;;       (if pass
    ;;           entry
    ;;           (random-instruction my-live-in)))
    ;;     (set! my-live-in (send machine update-live my-live-in new-entry))
    ;;     new-entry)
      
    ;;   (for/vector ([entry p]) (keep-or-new entry)))
    
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
          ;; (define new-p (fix-program p))
          ;; (values new-p
          ;;         (or (car (cost-all-inputs new-p w-error)) w-error))
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
	   (send simulator interpret program input)))
	(and program-out
	     (let ([t2 (current-milliseconds)]
		   [ret (correctness-cost output program-out constraint)]
		   [t3 (current-milliseconds)])
	       (send stat simulate (- t2 t1))
	       (send stat check (- t3 t2))
	       ret)))
      
      (define (cost-all-inputs program okay-cost)
        ;; (define correct 0)
        (define change-mode #f)
        ;; (for ([input inputs]
        ;;       [output outputs])
        ;;      (when correct
        ;;            (set! correct (+ correct (cost-one-input program input output)))
        ;;            (when (> correct okay-cost)
        ;;                  (set! correct #f))))

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
        (when debug (pretty-display `(final-correct ,correct)))

        (define ce #f)
        (when (and (number? correct) (= correct 0))
              (send stat inc-validate)
              (define t1 (current-milliseconds))
              (set! ce (send validator counterexample 
                             (vector-append prefix target postfix) (vector-append prefix program postfix)
                             constraint extra-info
                             #:assume assumption))
              (if ce 
                  (begin
                    (set! correct 1)
                    (set! ce (send simulator interpret prefix ce #:dep #f))
                    (set! inputs (cons ce inputs))
                    (set! outputs (cons (send simulator interpret target ce #:dep #t) 
                                        outputs))
                    (pretty-display (format "Add counterexample. Total = ~a." (length inputs)))
                    (send machine display-state ce)
		    (send printer print-syntax (send printer decode program))
                    )
                  (begin
                    (send stat inc-correct)
                    (when syn-mode (set! change-mode #t) (set! syn-mode #f))
                                        ;(send stat inc-correct)
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
              (when (and (= correct 0) (= total-cost (get-field best-correct-cost stat)))
                    (send stat update-best-correct-program program))
              (when (and (= correct 0) (< total-cost (get-field best-correct-cost stat)))
                    (pretty-display "NEW! best-correct-program")
                    (pretty-display "program-eq? --> true")
                    (pretty-display "target:")
                    (send printer print-struct target)
                    (pretty-display "candidate:")
                    (send printer print-struct program)
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
        (when (and update-size (<= (+ update-size 5) (vector-length current)))
              (pretty-display (format ">>> reduce size from ~a to ~a" 
                                      (vector-length current) update-size))
              (cond
               [syn-mode
                (define-values (new-p new-cost) 
                  (reduce-size current current-cost update-size))
                (set! current new-p)
                (set! current-cost new-cost)
                ;; (define tmp (send printer encode
                ;;                   (send parser ast-from-file
                ;;                         (format "~a/best.s" (get-field dir stat)))))
                ;; (send machine analyze-args (vector) tmp (vector))
                ]

               [else
                (pretty-display ">>> steal best program")
                (define-values (best-cost len time id) (send stat get-best-info-stat))
                (set! current
                      (send printer encode
                            (send parser ast-from-file 
                                  (format "~a/best.s" (get-field dir stat)))))
		;; (send machine analyze-args (vector) current (vector) #:only-const #t)
                (set! current-cost best-cost)
                ]
              ))
        (define t1 (current-milliseconds))
        (define proposal (mutate current))
        (define t2 (current-milliseconds))
        (send stat mutate (- t2 t1))
        (when debug
              (pretty-display (format "================ Current (syn=~a) =================" syn-mode))
              (send printer print-struct current)
              ;; (define cost (cost-all-inputs current (arithmetic-shift 1 32)))
              ;; (pretty-display (format "actual cost: ~a" cost))
              (pretty-display (format "================ Propose (syn=~a) =================" syn-mode))
              (send printer print-struct proposal)
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


      ;; (pretty-display `(cost-target ,(cost-all-inputs target 100)))
      ;; (raise "done")
      (with-handlers 
       ([exn:break? (lambda (e) (send stat print-stat-to-file))])
       
       (timeout time-limit
                (iter init 
                      ;; cost-all-inputs can return #f if program is invalid
                      (or (car (cost-all-inputs init w-error))
                          w-error)
                      ))
       )
      )


    (define (adjust cost val dep inter [min-val 1])
      ;;(pretty-display `(adjust ,cost ,val))
      (define (dfs x)
        ;;(pretty-display `(dfs ,(node-val x)))
        (if (member (node-val x) inter)
            (node-size x)
            (for/sum ([i (node-p x)])
                     (if (node? i) (dfs i) 0))))
      
      
      (define half (max (/ cost 2) min-val))
      (cond
       [base-cost cost]
       [(<= cost min-val) 
        ;;(pretty-display `(min-val))
        cost]
       ;; [(and (node? dep) (equal? (node-val dep) val) (member val inter))
       ;;  (pretty-display `(cover-all))
       ;;  min-val]
       [(node? dep)
        (define total (node-size dep))
        (define uncover (- total (dfs dep)))
        ;;(pretty-display `(uncover-total ,uncover ,total ,cost))
        ;;(exact->inexact (add1 (* (/ uncover total) (sub1 cost))))
        (exact->inexact (+ half (* (/ uncover total) (- cost half))))
        ]

       ;; No computation
       [else 
        ;;(pretty-display `(no-comp))
        min-val])
      )

    ))
