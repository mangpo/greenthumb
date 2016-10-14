#lang s-exp rosette

(require "../simulator-rosette.rkt" "../ops-rosette.rkt" "../inst.rkt" "$-machine.rkt")
(provide $-simulator-rosette%)

(define $-simulator-rosette%
  (class simulator-rosette%
    (super-new)
    (init-field machine)
    (override interpret performance-cost get-constructor)

    (define (get-constructor) $-simulator-rosette%)

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
    (define bvshl  (bvop shl))
    (define bvshr  (bvop >>))   ;; signed shift right
    (define bvushr (bvop ushr)) ;; unsigned shift right
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;; Required methods ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Interpret a given program from a given state.
    ;; 'program' is a vector of 'inst' struct.
    ;; 'ref' is optional. When given, it is an output program state returned from spec.
    ;; We can assert something from ref to terminate interpret early.
    ;; This can help prune the search space.
    (define (interpret program state [ref #f])
      ? ;; modify this function

      ;; Example:
      
      ;; Copy vector before modifying it because vector is mutable, and
      ;; we don't want to mutate the input state.
      (define regs-out (vector-copy (progstate-regs state)))
      ;; Set mem = #f for now.
      (define mem #f)

      ;; Call this function when we want to reference mem. This function will clone the memory.
      ;; We do this instead of cloning memory at the begining
      ;; because we want to avoid creating new memory object when we can.
      (define (prepare-mem)
          ;; When referencing memory object, use send* instead of send to make it compatible with Rosette.
          (unless mem
                  (set! mem (send* (progstate-memory state) clone (and ref (progstate-memory ref))))))
        

      (define (interpret-inst my-inst)
        (define op (inst-op my-inst))
        (define op-name (vector-ref opcodes op))
        (define args (inst-args my-inst))
        
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

        (define (load)
          (define d (vector-ref args 0))
          (define a (vector-ref args 1))
          (prepare-mem)
          ;; When referencing memory object, use send* instead of send to make it compatible with Rosette.
          (vector-set! regs-out d (send* mem load (vector-ref regs-out a))))

        (define (store)
          (define val (vector-ref args 0))
          (define addr (vector-ref args 1))
          (prepare-mem)
          ;; When referencing memory object, use send* instead of send to make it compatible with Rosette.
          (send* mem store (vector-ref regs-out addr) (vector-ref regs-out val)))

        (cond
         [(equal? op-name `nop)   (void)]
         [(equal? op-name `add)   (rrr bvadd)]
         [(equal? op-name `add#)  (rri bvadd)]
         [(equal? op-name `shl#)  (rri bvshl)]
         [(equal? op-name `store) (store)]
         [(equal? op-name `load)  (load)]
         [else (assert #f (format "simulator: undefine instruction ~a" op))]))
      ;; end interpret-inst

      (for ([x program]) (interpret-inst x))
      
      ;; If mem = #f (never reference mem), set mem before returning.
      (unless mem (set! mem (progstate-memory state)))
      (progstate regs-out mem)
      )

    ;; Estimate performance cost of a given program.
    (define (performance-cost program)
      ? ;; modify this function

      ;; Example:
      (define cost 0)
      (for ([x program])
           ;; GreenThumb set nop-id automatically from opcode `nop
	   (unless (= (inst-op x) nop-id) (set! cost (add1 cost))))
      cost)

    ))
