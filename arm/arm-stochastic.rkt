#lang racket

(require "../stochastic.rkt"
         "../ast.rkt" "arm-ast.rkt"
         "../machine.rkt" "arm-machine.rkt" 
         "arm-simulator-racket.rkt" "arm-solver.rkt")

(provide arm-stochastic%)

(define arm-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine printer solver simulator stat mutate-dist live-in base-cost)
    (inherit random-args-from-op mutate filter-live update-live adjust)
    (override correctness-cost get-arg-ranges 
	      get-mutations random-instruction mutate-other
	      inst-copy-with-op inst-copy-with-args
              get-operand-live)

    (set! solver (new arm-solver% [machine machine] [printer printer]))
    (set! simulator (new arm-simulator-racket% [machine machine]))

    (set! mutate-dist
	  #hash((opcode . 2) (operand . 2) (swap . 1) (instruction . 2)
		(shf . 4) (cond-type . 2)))
	  

    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define shf-inst-id (get-field shf-inst-id machine))
    (define inst-with-shf (get-field inst-with-shf machine))
    (define nregs (send machine get-nregs))
    (define nmems (send machine get-nmems))

    (define reg-range (list->vector (range nregs)))
    (define operand2-range
      (list->vector
       (append (range bit) (list #x3f #xff0000 #xff00 (- #xff000000) (- #x80000000)))))
    ;; (for/list ([i (range 5 (sub1 bit))]) 
    ;;           (arithmetic-shift 1 i))
    ;; (list (- (arithmetic-shift 1 (sub1 bit)))))))
    (define const-range
      (list->vector
       (append (range 17) (list (sub1 bit) 
                                #x1111 #x3333 #x5555 #xaaaa #xcccc
                                #xf0f0 #x0f0f #x3f 
				#xffff #xaaab #x2aaa #xfff4))))
    ;; (for/list ([i (range 5 (quotient bit 2))]) 
    ;;           (arithmetic-shift 1 i)))))
    
    (define bit-range (list->vector (range bit)))
    (define mem-range (list->vector (for/list ([i (range 1 11)]) (- i))))

    (define (inst-copy-with-op x op) 
      (define opname (vector-ref inst-id op))
      (if (member opname inst-with-shf)
          (arm-inst op (inst-args x) (inst-shfop x) (inst-shfarg x) (inst-cond x))
          (arm-inst op (inst-args x) #f #f (inst-cond x))))

    (define (inst-copy-with-args x args) 
      (arm-inst (inst-op x) args (inst-shfop x) (inst-shfarg x) (inst-cond x)))

    (define (get-operand-live state)
      (and state
           (let ([regs (progstate-regs state)]
                 [live (list)])
             (for ([i (vector-length regs)]
                   [r regs])
                  (when r (set! live (cons i live))))
             live)))
          

    (define (get-mutations opcode-name)
      ;;(pretty-display `(get-mutations ,opcode-name ,(vector-member opcode-name shf-inst-id) ,shf-inst-id))
      (define mutations '(instruction swap))
      (unless (equal? opcode-name `nop)
              (set! mutations (cons `opcode mutations))
              (set! mutations (cons `operand mutations)))
      (when (member opcode-name inst-with-shf)
	    (set! mutations (cons `shf mutations)))
      (unless (member opcode-name '(tst cmp tst# cmp# nop))
              (set! mutations (cons `cond-type mutations)))
      mutations)

    (define (mutate-other index entry p type)
      (cond
       [(equal? type `shf)       (mutate-shf index entry p)]
       [(equal? type `cond-type) (mutate-cond index entry p)]
       [else (raise (format "No support for mutation ~a" type))]))


    (define (mutate-shf index entry p)
      (define opcode-id (inst-op entry))
      (define args (vector-copy (inst-args entry)))
      (define len (vector-length shf-inst-id))

      (define shfop (inst-shfop entry))
      (define shfop-name (and shfop (vector-ref shf-inst-id shfop)))
      (define shfarg (inst-shfarg entry))
      (define rand (random)) ;; op 0.5, arg 0.25, all 0.05, nop 0.2
      (when debug
	    (pretty-display (format " >> shf-type = ~a" rand)))
      (cond
       [(or (equal? shfop-name #f) (equal? shfop-name `nop) (< rand 0.5))
        (define my-live-in live-in)
        (for ([i index])
             (set! my-live-in (update-live my-live-in (vector-ref p i))))
	(set! shfop (vector-member (random-from-vec-ex shf-inst-id `nop) shf-inst-id))
	(set! shfarg (random-from-vec (get-shfarg-range shfop my-live-in)))]

       [(< rand 0.55) ;; op
	(define my-list
	  (if (member shfop-name '(asr lsr lsl)) '(asr lsr lsl) '(asr# lsr# lsl#)))
	(set! shfop (vector-member 
		     (random-from-list-ex my-list shfop-name)
		     shf-inst-id))]

       [(< rand 0.8) ;; arg
        (define my-live-in live-in)
        (for ([i index])
             (set! my-live-in (update-live my-live-in (vector-ref p i))))
	(set! shfarg (random-from-vec-ex (get-shfarg-range shfop my-live-in) shfarg))]

       [else ;; nop
	(set! shfop #f)
	(set! shfarg #f)])

      (define new-entry (arm-inst (inst-op entry) (inst-args entry)
				  shfop shfarg (inst-cond entry)))
      (define new-p (vector-copy p))
      (vector-set! new-p index new-entry)
      (send stat inc-propose `shf)
      new-p)

    (define (mutate-cond index entry p)
      (define cmp 
	(for/or ([i (reverse (range index))])
		(let ([name-i (vector-ref inst-id (inst-op (vector-ref p i)))])
		  (and (member name-i '(tst cmp tst# cmp#)) name-i))))

      (cond
       [cmp 
	(define cond-type (inst-cond entry))
	(define new-cond-type 
	  (if (member cmp '(tst tst#))
	      (random-from-list-ex (range -1 2) cond-type)
	      (random-from-list-ex (range -1 6) cond-type)))
	(define new-entry (struct-copy arm-inst entry [cond new-cond-type]))
	(when debug
	      (pretty-display (format " --> cond-type = ~a --> ~a" cond-type new-cond-type)))
	(define new-p (vector-copy p))
	(vector-set! new-p index new-entry)
	(send stat inc-propose `cond-type)
	new-p]
       [else (mutate p)]))

    (define (random-instruction live-in [opcode-id (random (vector-length inst-id))])
      
      (define opcode-name (vector-ref inst-id opcode-id))
      (define args (random-args-from-op opcode-name live-in))
      (define shf? (and (member opcode-name inst-with-shf) (< (random) 0.3)))
      (define shfop (and shf? (random (vector-length shf-inst-id))))
      (define shfarg-range (and shf? (get-shfarg-range shfop live-in)))
      (define shfarg (and shf? (vector-ref shfarg-range (random (vector-length shfarg-range)))))
      (define cond-type (sub1 (random 7)))
      (arm-inst opcode-id args shfop shfarg cond-type))

    (define (get-shfarg-range shfop-id live-in)
      (define shfop-name (vector-ref shf-inst-id shfop-id))
      (if (member shfop-name '(asr lsr lsl)) (filter-live reg-range live-in) bit-range))

    ;; nargs, ranges
    (define (get-arg-ranges opcode-name entry live-in)
      (define-syntax-rule (reg)
        (filter-live reg-range live-in))
      (define class-id (send machine get-class-id opcode-name))
      ;;(pretty-display `(get-arg-ranges ,opcode-name ,class-id))
      (cond
       [(equal? class-id 0) (vector reg-range (reg) (reg))]
       [(equal? class-id 1) (vector reg-range (reg) operand2-range)]
       ;[(equal? class-id 2) (vector reg-range (reg) bit-range)]
       [(equal? class-id 2) (vector reg-range (reg))]
       [(equal? class-id 3) (vector reg-range const-range)]
       [(equal? class-id 4) (vector reg-range reg-range (reg) (reg))]
       [(equal? class-id 5) (vector reg-range (reg) bit-range bit-range)]
       [(equal? class-id 6) (vector reg-range reg-range mem-range)]
       [(equal? opcode-name `bfc) (vector (reg) bit-range bit-range)]
       [else (vector)]))

    ;; state1: reference
    ;; state2: proposal
    (define (correctness-cost state1 state2 constraint)
      (define (diff-cost x y)
        (define (pop-count a)
          (set! a (- a (bitwise-and (arithmetic-shift a -1) #x55555555)))
          ;;(pretty-display a)
          (set! a (+ (bitwise-and a #x33333333)
                     (bitwise-and (arithmetic-shift a -2) #x33333333)))
          ;;(pretty-display a)
          (set! a (bitwise-and (+ a (arithmetic-shift a -4)) #x0f0f0f0f))
          (set! a (+ a (arithmetic-shift a -8)))
          (set! a (+ a (arithmetic-shift a -16)))
          (bitwise-and a #x3f))

        (pop-count (bitwise-xor (bitwise-and x #xffffffff) 
                                (bitwise-and y #xffffffff))))
      
      (define regs (progstate-regs constraint))
      (define memory (progstate-memory constraint))
      (define len (vector-length memory))
      (define z (progstate-z constraint))

      (define regs1 (progstate-regs state1))
      (define memory1 (progstate-memory state1))
      (define dep (progstate+-extra state1))
      (define regs1-dep (progstate-regs dep))
      (define memory1-dep (progstate-memory dep))
      (define z1 (progstate-z state1))

      (define regs2 (progstate-regs state2))
      (define memory2 (progstate-memory state2))
      (define inter (progstate+-extra state2))
      (define z2 (progstate-z state2))
      
      (define correctness 0)
      (define relax base-cost)
      (define misalign-penalty 1)
      (define misalign 0)
      (if relax
          (for ([r regs]
                [i (vector-length regs)])
               (when r
                     (let* ([r1 (vector-ref regs1 i)]
                            [cost-r (diff-cost r1 (vector-ref regs2 i))]
                            [best-j i])
                       (for ([j (vector-length regs)])
                            (let* ([r2 (vector-ref regs2 j)]
                                   [this-cost (diff-cost r1 r2)])
                              (when (< this-cost cost-r)
                                    (set! cost-r this-cost)
                                    (set! best-j j)
                                    )))
                       (set! correctness (+ correctness cost-r))
                       (unless (= best-j i) (set! misalign (+ misalign misalign-penalty)))
                       )))
          
          (for ([r regs]
                [r1 regs1]
                [r2 regs2]
                [r1-dep regs1-dep]
                [i (vector-length regs)])
               (when r 
                     ;;(pretty-display `(correctness reg ,i))
                     (let ([my-cost (diff-cost r1 r2)])
                       (set! correctness (+ correctness (adjust my-cost r1 r1-dep inter)))
                       ;; (set! correctness (+ correctness my-cost))
                       ))))

      (for ([m memory]
            [m1 memory1]
            [m2 memory2]
            [m1-dep memory1-dep])
           (when m 
                 (let ([my-cost (diff-cost m1 m2)])
                   (set! correctness (+ correctness (adjust my-cost m1 m1-dep inter))))
                   ;;(set! correctness (+ correctness my-cost))
                   ))

      (when (and z (not (equal? z1 z2))) (set! correctness (add1 correctness)))

      ;; (when debug
      ;;       (pretty-display `(correct ,(vector-ref regs1 0) ,(vector-ref regs2 0) ,correctness)))
      
      ;; (when (= correctness 0)
      ;;       (if (= misalign 0)
      ;;           (void)
      ;;           (send stat inc-misalign)))
      ;; (+ correctness misalign)
      (+ correctness misalign)
      )
    ))




  
