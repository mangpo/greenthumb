#lang racket

(require "../stochastic.rkt"
         "../ast.rkt" "arm-ast.rkt"
         "../machine.rkt" "arm-machine.rkt" 
         "arm-simulator-racket.rkt" "arm-validator.rkt")

(provide arm-stochastic%)

(define arm-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine printer validator simulator stat mutate-dist live-in)
    (inherit random-args-from-op mutate)
    (override correctness-cost 
	      get-mutations random-instruction mutate-other mutate-swap
	      inst-copy-with-op inst-copy-with-args)

    (set! validator (new arm-validator% [machine machine] [printer printer]))
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

    (define-syntax-rule (get-shfarg-range shfop my-live-in)
      (send machine get-shfarg-range shfop my-live-in))

    (define (inst-copy-with-op x op) 
      (define opname (vector-ref inst-id op))
      (if (member opname inst-with-shf)
          (arm-inst op (inst-args x) (inst-shfop x) (inst-shfarg x) (inst-cond x))
          (arm-inst op (inst-args x) #f #f (inst-cond x))))

    (define (inst-copy-with-args x args) 
      (arm-inst (inst-op x) args (inst-shfop x) (inst-shfarg x) (inst-cond x)))

    ;; (overriden) Get a list of valid mutations given an opcode.
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
    
    ;; (overriden) Mutate by swapping 2 instructions.
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

      ;; Only swap if operands of instruction at index-large are live at index-small.
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

    (define (mutate-other index entry p type)
      (cond
       [(equal? type `shf)       (mutate-shf index entry p)]
       [(equal? type `cond-type) (mutate-cond index entry p)]
       [else (raise (format "No support for mutation ~a" type))]))

    ;; Mutate optinal shift.
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
             (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
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
             (set! my-live-in (send machine update-live my-live-in (vector-ref p i))))
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

    ;; Mutate conditional-code suffix.
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
	      (random-from-list-ex (range 3) cond-type)
	      (random-from-list-ex (range 7) cond-type)))
	(define new-entry (struct-copy arm-inst entry [cond new-cond-type]))
	(when debug
	      (pretty-display (format " --> cond-type = ~a --> ~a" cond-type new-cond-type)))
	(define new-p (vector-copy p))
	(vector-set! new-p index new-entry)
	(send stat inc-propose `cond-type)
	new-p]
       [else (mutate p)]))

    ;; (overriden) Create a new instruction with operands that are live (in live-in) and with opcode-id if specified.
    ;; live-in: compact format
    (define (random-instruction live-in [opcode-id (random-from-list (get-field inst-pool machine))])
      (define opcode-name (vector-ref inst-id opcode-id))
      (define args (random-args-from-op opcode-name live-in))
      (cond
       [args
        (define shf? (and (member opcode-name inst-with-shf) (< (random) 0.3)))
        (define shfop (and shf? (random (vector-length shf-inst-id))))
        (define shfarg-range (and shf? (get-shfarg-range shfop live-in)))
        (define shfarg (and shf? (vector-ref shfarg-range (random (vector-length shfarg-range)))))
        (define cond-type (random 7))
        (arm-inst opcode-id args shfop shfarg cond-type)]
       [else (random-instruction live-in)])
      )
    
    ;; Compute correctness cost sum of all bit difference in live registers and memory.
    ;; state1: expected in progstate format
    ;; state2: actual in progstate format
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
      (define z1 (progstate-z state1))

      (define regs2 (progstate-regs state2))
      (define memory2 (progstate-memory state2))
      (define z2 (progstate-z state2))
      
      (define correctness 0)
      (define misalign-penalty 1)
      (define misalign 0)
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

      (for ([m memory]
            [m1 memory1]
            [m2 memory2])
           (when m 
                 (let ([my-cost (diff-cost m1 m2)])
                   (set! correctness (+ correctness my-cost))
                   )))

      (when (and z (not (equal? z1 z2))) (set! correctness (add1 correctness)))

      ;; (when debug
      ;;       (pretty-display `(correct ,(vector-ref regs1 0) ,(vector-ref regs2 0) ,correctness)))
      
      ;; (when (= correctness 0)
      ;;       (if (= misalign 0)
      ;;           (void)
      ;;           (send stat inc-misalign)))
      (+ correctness misalign)
      )
    ))




  
