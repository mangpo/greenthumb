#lang racket

(require "../stochastic.rkt"
         "../inst.rkt"
         "GA-machine.rkt" "GA-ops-racket.rkt"
         "GA-simulator-racket.rkt")

(provide GA-stochastic%)

;(define-syntax-rule (min a b ...) a)

(define GA-stochastic%
  (class stochastic%
    (super-new)
    (inherit-field machine stat mutate-dist nop-mass)
    (inherit pop-count32)
    (override correctness-cost 
	      get-mutations mutate-other)

    (set! mutate-dist 
      #hash((opcode . 1) (operand . 1) (swap . 1) (instruction . 1) (rotate . 1)))
    (set! nop-mass 0.4)

    (define bit (get-field bitwidth machine))
    (define opcodes (get-field opcodes machine))

    (define (get-mutations opcode-id)
      (define mutations (super get-mutations opcode-id))
      (cons 'rotate mutations))

    (define (mutate-other index entry p type)
      (cond
       [(equal? type `rotate) (mutate-rotate index entry p)]
       [else (raise (format "mutate-other: undefined mutate ~a" type))]))

    (define (mutate-rotate index entry p)
      (send stat inc-propose `rotate)
      (vector-append (vector-copy p 0 index) 
                     (vector-copy p (add1 index))
                     (vector entry)))

    ;; state1: reference
    ;; state2: check
    (define (correctness-cost state1 state2 constraint)

      (define correctness 0)
      (define (diff-cost x y)
        (pop-count32 (bitwise-xor (bitwise-and x #x3ffff) 
                                  (bitwise-and y #x3ffff))))
      (define-syntax-rule (accum x) (set! correctness (+ correctness x)))

      (define-syntax-rule (check-reg progstate-x)
	(when (progstate-x constraint)
              (accum
               (min
                (diff-cost (progstate-x state1) (progstate-x state2))
                (add1 (diff-cost (progstate-x state1) (progstate-t state2))))
               )))

      (define (check-reg-t)
	(when (progstate-t constraint)
              ;; load mem
              (accum
               (min
                (diff-cost (progstate-t state1) (progstate-t state2))
                (add1 (diff-cost (progstate-t state1) (progstate-a state2)))
                (add1 (diff-cost (progstate-t state1) (progstate-r state2)))
                (add1 (diff-cost (progstate-t state1) (progstate-s state2))))
               )))

      (define (check-reg-s)
	(when (progstate-s constraint)
              (accum
               (min
                (diff-cost (progstate-s state1) (progstate-s state2))
                (add1 (diff-cost (progstate-s state1) (progstate-t state2)))
                (add1 (diff-cost (progstate-s state1) (get-stack (progstate-data state2) 0))))
               )))

      (define (check-reg-r)
	(when (progstate-r constraint)
              (accum
               (min
                (diff-cost (progstate-r state1) (progstate-r state2))
                (add1 (diff-cost (progstate-r state1) (progstate-t state2)))
                (add1 (diff-cost (progstate-r state1) (get-stack (progstate-return state2) 0))))
               )))
      
      (define-syntax-rule (check-stack progstate-x)
	(when (progstate-x constraint)
	      (for ([i (in-range (progstate-x constraint))])
		   (let ([val1 (get-stack (progstate-x state1) i)]
			 [val2 (get-stack (progstate-x state2) i)])
		     (accum (diff-cost val1 val2))))))
      
      (define-syntax-rule (check-mem)
	(let ([mem1 (progstate-memory state1)]
	      [mem2 (progstate-memory state2)]
	      [mem-const (progstate-memory constraint)])
          (when mem-const (accum (send mem1 correctness-cost mem2 diff-cost bit)))))
      
      (define-syntax-rule (check-comm)
	(when (progstate-comm constraint)
	      (let ([comm1 (progstate-comm state1)]
		    [comm2 (progstate-comm state2)])
                (accum (send comm1 correctness-cost comm2 diff-cost bit)))))
      
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
      
