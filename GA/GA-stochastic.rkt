#lang racket

(require "../stochastic.rkt"
         "../ast.rkt"
         "../machine.rkt" "GA-machine.rkt" 
         "GA-simulator-racket.rkt" "GA-solver.rkt")

(provide GA-stochastic%)

(define GA-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine printer solver simulator stat mutate-dist nop-mass)
    (override get-mutations mutate-operand mutate-other
              correctness-cost get-arg-ranges random-instruction)

    (set! mutate-dist 
      #hash((opcode . 1) (operand . 1) (swap . 1) (instruction . 1) (rotate . 1)))
    (set! nop-mass 0.4)
    (set! solver (new GA-solver% [machine machine] [printer printer]))
    (set! simulator (new GA-simulator-racket% [machine machine]))

    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define nmems (send machine get-nmems))

    (define (get-mutations opcode-name)
      (if (equal? opcode-name `@p) 
	  '(operand swap instruction rotate)
	  '(opcode swap instruction rotate)))

    (define const-range 
          (list->vector
           (append (range -16 17) (list (sub1 bit) UP DOWN LEFT RIGHT IO)
                   (for/list ([i (range 5 (sub1 (quotient bit 2)))]) 
                             (arithmetic-shift 1 i))
                   (list (- (arithmetic-shift 1 (sub1 (quotient bit 2))))))))
    
    (define (mutate-other index entry p type)
      (cond
       [(equal? type `rotate) (mutate-rotate index entry p)]
       [else (raise (format "mutate-other: undefined mutate ~a" type))]))

    (define (mutate-rotate index entry p)
      (send stat inc-propose `rotate)
      (if (= (random 2) 0)
          (vector-append (vector-copy p 0 index) 
                         (vector-copy p (add1 index))
                         (vector entry))
          (let ([last-index (sub1 (vector-length p))])
            (vector-append (vector-copy p 0 index)
                           (vector-copy p last-index)
                           (vector-copy p index last-index)))))
      

    (define (mutate-operand index entry p)
      (send stat inc-propose `operand)
      (define opcode-id (inst-op entry))
      (define arg (inst-args entry))
      (define new-p (vector-copy p))
      (vector-set! new-p index (inst opcode-id (random-from-vec-ex const-range arg)))
      new-p)
    
    (define (random-instruction [opcode-id (random (vector-length inst-id))])
      (define opcode-name (vector-ref inst-id opcode-id))
      (define arg (and (equal? opcode-name `@p) (random-from-vec const-range)))
      (inst opcode-id arg))
    
    (define (get-arg-ranges opcode-name entry)
      (raise "GA: get-arg-ranges should not be called."))

    ;; state1: reference
    ;; state2: check
    (define (correctness-cost state1 state2 constraint)
      (define correctness 0)
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

        (pop-count (bitwise-xor (bitwise-and x #x3ffff) 
                                (bitwise-and y #x3ffff))))

      (define-syntax-rule (accum x) (set! correctness (+ correctness x)))

      (define (check-reg progstate-x)
	(when (progstate-x constraint)
              (accum
               (min
                (diff-cost (progstate-x state1) (progstate-x state2))
                (add1 (diff-cost (progstate-x state1) (progstate-t state2)))
                ))))

      (define (check-reg-t)
	(when (progstate-t constraint)
              ;; load mem
              (accum
               (min
                (diff-cost (progstate-t state1) (progstate-t state2))
                (add1 (diff-cost (progstate-t state1) (progstate-a state2)))
                (add1 (diff-cost (progstate-t state1) (progstate-r state2)))
                (add1 (diff-cost (progstate-t state1) (progstate-s state2)))
                ))))

      (define (check-reg-s)
	(when (progstate-s constraint)
              (accum
               (min
                (diff-cost (progstate-s state1) (progstate-s state2))
                (add1 (diff-cost (progstate-s state1) (progstate-t state2)))
                (add1 (diff-cost (progstate-s state1) (get-stack (progstate-data state2) 0)))
                ))))

      (define (check-reg-r)
	(when (progstate-r constraint)
              (accum
               (min
                (diff-cost (progstate-r state1) (progstate-r state2))
                (add1 (diff-cost (progstate-r state1) (progstate-t state2)))
                (add1 (diff-cost (progstate-r state1) (get-stack (progstate-return state2) 0)))
                ))))
      
      (define-syntax-rule (check-stack progstate-x)
	(when (progstate-x constraint)
	      (for ([i (in-range (progstate-x constraint))])
                   (accum
                    (diff-cost (get-stack (progstate-x state1) i) 
                               (get-stack (progstate-x state2) i))))))
      
      (define-syntax-rule (check-mem)
	(let ([mem1 (progstate-memory state1)]
	      [mem2 (progstate-memory state2)]
	      [mem-const (progstate-memory constraint)])
          (unless (vector? mem-const)
                  (set! mem-const (make-vector (vector-length mem1) mem-const)))
          (for ([i (in-range 0 (vector-length mem1))])
               (when (vector-ref mem-const i)
                     (accum
                      (min
                       (diff-cost (vector-ref mem1 i) (vector-ref mem2 i))
                       (+ 1 (diff-cost (vector-ref mem1 i) (progstate-t state2))
                          (if (or (= i (progstate-a state2)) (= i (progstate-b state2)))
                              0 1))
                       ))))))
      
      (define-syntax-rule (check-comm)
	(when (progstate-comm constraint)
	      (accum
               (* bit (abs (- (length (progstate-comm state1)) 
                              (length (progstate-comm state2))))))
	      (for ([i1 (progstate-comm state1)]
		    [i2 (progstate-comm state2)])
		   (accum (diff-cost (first i1) (first i2)))
		   (unless (= (second i1) (second i2))
			   (accum 1))
		   (unless (= (third i1) (third i2))
			   (accum 1)))))
      
      (check-reg progstate-a)
      (check-reg progstate-b)
      (check-reg-r)
      (check-reg-s)
      (check-reg-t)
      (check-stack progstate-data)
      (check-stack progstate-return)
      (check-mem)
      (check-comm)

      correctness
      )

    ))
      
