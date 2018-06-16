#lang racket

(require "../simulator-racket.rkt" "../ops-racket.rkt" "../inst.rkt" "ptx-machine.rkt")
(provide ptx-simulator-racket%)

(define ptx-simulator-racket%
  (class simulator-racket%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) ptx-simulator-racket%)

    (define bit (get-field bitwidth machine))
    (define nop-id (get-field nop-id machine))
    (define opcodes (get-field opcodes machine))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Truncate x to 'bit' bits and convert to signed number.
    ;; Always use this macro when interpreting an operator.
    (define-syntax-rule (finitize-bit x) (finitize x bit))
    (define-syntax-rule (bvop op)     
      (lambda (x y) (finitize-bit (op x y))))
    (define (shl a b) (<< a b bit))
    (define (ushr a b) (>>> a b bit))

    ;; Binary operation.
    (define bvadd  (bvop +))
    (define bvsub  (bvop -))
    (define bvmul  (bvop *))
    (define bvshl  (bvop shl))
    (define bvshr  (bvop >>))   ;; signed shift right
    (define bvushr (bvop ushr)) ;; unsigned shift right

    (define (bvsmmul x y) (smmul x y bit))
    (define (bvummul x y) (ummul x y bit))
    
    (define bvand (bvop bitwise-and))
    (define bvrem (bvop remainder))
    (define (bveq x y) (if (= x y) 1 0))
    (define (bvnot x) (if (= x 0) 1 0))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Required methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Interpret a given program from a given state.
    ;; 'program' is a vector of 'inst' struct.
    ;; 'ref' is optional. When given, it is an output program state returned from spec.
    ;; We can assert something from ref to terminate interpret early.
    ;; This can help prune the search space.
    (define (interpret program state [ref #f])
      
      ;; Copy vector before modifying it because vector is mutable, and
      ;; we don't want to mutate the input state.
      (define regs-out (vector-copy (progstate-regs state)))
      (define preds-out (vector-copy (progstate-preds state)))
      ;; Set mem = #f for now.
      (define mem #f)

      (define (interpret-inst my-inst)
        (define op (inst-op my-inst))
        (define op-name (vector-ref opcodes op))
        (define args (inst-args my-inst))
        
        (define (rr f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define val (f (vector-ref regs-out a))) ;; reg [op] reg
          (vector-set! regs-out d val))
        
        (define (pp f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define val (f (vector-ref preds-out a))) ;; reg [op] reg
          (vector-set! preds-out d val))
        
        (define (rrr f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (f (vector-ref regs-out a) (vector-ref regs-out b))) ;; reg [op] reg
          (vector-set! regs-out d val))
        
        (define (rri f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (f (vector-ref regs-out a) b)) ;; reg [op] const
          (vector-set! regs-out d val))
        
        (define (pri f)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
          (define val (f (vector-ref regs-out a) b)) ;; reg [op] const
          (vector-set! preds-out d val))
        
        (define (ddri f-lo f-hi)
          (define d-lo (vector-ref args 0))
          (define d-hi (vector-ref args 1))
          (define a (vector-ref args 2))
          (define b (vector-ref args 3))
	  (define val-lo (f-lo (vector-ref regs-out a) b))
	  (define val-hi (f-hi (vector-ref regs-out a) b))
	  (vector-set! regs-out d-lo val-lo)
	  (vector-set! regs-out d-hi val-hi))

	(define (selp)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (define b (vector-ref args 2))
	  (define p (vector-ref args 3))
	  (vector-set! regs-out d
		       (if (> (vector-ref preds-out p) 0)
			   (vector-ref regs-out a) (vector-ref regs-out b))))

        (cond
         [(equal? op-name `nop)   (void)]

         [(equal? op-name `not)   (pp bvnot)]
         [(equal? op-name `mov)   (rr identity)]
	 
         [(equal? op-name `add)   (rrr bvadd)]
         [(equal? op-name `sub)   (rrr bvsub)]
         [(equal? op-name `add#)  (rri bvadd)]
         [(equal? op-name `mul.lo#)  (rri bvmul)]
         [(equal? op-name `and#)  (rri bvand)]
         [(equal? op-name `rem#)  (rri bvrem)]
         [(equal? op-name `shl#)  (rri bvshl)]
         [(equal? op-name `shr#)  (rri bvushr)]
	 
         [(equal? op-name `setp.eq#)   (pri bveq)]
	 
         [(equal? op-name `mul.wide#)  (ddri bvmul bvummul)]
	 
         [(equal? op-name `selp)  (selp)]
	 
         [else (assert #f (format "simulator: undefine instruction ~a" op))]))
      ;; end interpret-inst

      (for ([x program]) (interpret-inst x))
      (progstate regs-out preds-out)
      )

    ;; Estimate performance cost of a given program.
    (define (performance-cost program)
      ;; Example:
      (define cost 0)
      (for ([x program])
           ;; GreenThumb set nop-id automatically from opcode `nop
	   (unless (= (inst-op x) nop-id) (set! cost (add1 cost))))
      cost)

    ))

