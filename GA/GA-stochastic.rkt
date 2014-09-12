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
    (override get-mutations mutate-operand
              correctness-cost get-arg-ranges random-instruction)

    (set! mutate-dist 
      #hash((operand . 1) (swap . 1) (instruction . 1)))
    (set! nop-mass 0.4)
    (set! solver (new GA-solver% [machine machine] [printer printer]))
    (set! simulator (new GA-simulator-racket% [machine machine]))

    (define bit (get-field bit machine))
    (define inst-id (get-field inst-id machine))
    (define nmems (send machine get-nmems))

    (define (get-mutations opcode-name)
      (if (equal? opcode-name `@p) 
	  '(operand swap instruction)
	  '(swap instruction)))

    (define const-range 
          (list->vector
           (append (range -16 17) (list (sub1 bit) UP DOWN LEFT RIGHT IO)
                   (for/list ([i (range 5 (sub1 (quotient bit 2)))]) 
                             (arithmetic-shift 1 i))
                   (list (- (arithmetic-shift 1 (sub1 (quotient bit 2))))))))

    (define (mutate-operand index entry p)
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

	(set! correctness
	      (+ correctness
		 (pop-count (bitwise-xor (bitwise-and x #x3ffff) 
					 (bitwise-and y #x3ffff))))))

      (define (check-reg progstate-x)
	(when (progstate-x constraint)
	      (diff-cost (progstate-x state1) (progstate-x state2))))
      
      (define-syntax-rule (check-stack progstate-x)
	(when (progstate-x constraint)
	      (for ([i (in-range (progstate-x constraint))])
		   (diff-cost (get-stack (progstate-x state1) i) 
			      (get-stack (progstate-x state2) i)))))
      
      (define-syntax-rule (check-mem)
	(let ([mem1 (progstate-memory state1)]
	      [mem2 (progstate-memory state2)]
	      [mem-const (progstate-memory constraint)])
	  (if (vector? mem-const)
	      (for ([i (in-range 0 (vector-length mem1))])
		   (when (vector-ref mem-const i)
			 (diff-cost (vector-ref mem1 i) (vector-ref mem2 i))))
	      (when mem-const
		    (for ([i (in-range 0 (vector-length mem1))])
			 (diff-cost (vector-ref mem1 i) (vector-ref mem2 i)))))))
      
      (define-syntax-rule (check-comm)
	(when (progstate-comm constraint)
	      (set! correctness 
		    (+ correctness
		       (* bit
			  (abs (- (length (progstate-comm state1)) 
				  (length (progstate-comm state2)))))))
	      (for ([i1 (progstate-comm state1)]
		    [i2 (progstate-comm state2)])
		   (diff-cost (car i1) (car i2))
		   (unless (= (cdr i1) (cdr i2))
			   (set! correctness (add1 correctness))))))

      (check-reg progstate-a)
      (check-reg progstate-b)
      (check-reg progstate-r)
      (check-reg progstate-s)
      (check-reg progstate-t)
      (check-stack progstate-data)
      (check-stack progstate-return)
      (check-mem)
      (check-comm)

      correctness
      )

    ))
      
