#lang racket

(require "../machine.rkt" "../special.rkt")

(provide ptx-machine%  (all-defined-out))

;;;;;;;;;;;;;;;;;;;;; program state macro ;;;;;;;;;;;;;;;;;;;;;;;;
;; This is just for convenience.
(define-syntax-rule
  (progstate regs preds)
  (vector regs preds))

(define-syntax-rule (progstate-regs x) (vector-ref x 0))
(define-syntax-rule (progstate-preds x) (vector-ref x 1))

(define-syntax-rule (set-progstate-regs! x v) (vector-set! x 0 v))
(define-syntax-rule (set-progstate-preds! x v) (vector-set! x 1 v))

(define ptx-machine%
  (class machine%
    (super-new)
    (inherit-field bitwidth random-input-bits config)
    (inherit init-machine-description define-instruction-class finalize-machine-description
             define-progstate-type define-arg-type
             update-progstate-ins kill-outs)
    (override get-constructor progstate-structure)

    (define (get-constructor) ptx-machine%)
    
    (unless bitwidth (set! bitwidth 32))
    (set! random-input-bits bitwidth)

    ;;;;;;;;;;;;;;;;;;;;; program state ;;;;;;;;;;;;;;;;;;;;;;;;

    (define (progstate-structure)
      (progstate (for/vector ([i (car config)]) 'reg)
		 (for/vector ([i (cdr config)]) 'pred)
		 ))

    (define-progstate-type
      'reg 
      #:get (lambda (state arg) (vector-ref (progstate-regs state) arg))
      #:set (lambda (state arg val) (vector-set! (progstate-regs state) arg val)))
    
    (define-progstate-type
      'pred 
      #:get (lambda (state arg) (vector-ref (progstate-preds state) arg))
      #:set (lambda (state arg val) (vector-set! (progstate-preds state) arg val)))

    ;;;;;;;;;;;;;;;;;;;;; instruction classes ;;;;;;;;;;;;;;;;;;;;;;;;
    (define-arg-type 'reg (lambda (config) (range (car config))))
    (define-arg-type 'pred (lambda (config) (range (cdr config))))
    (define-arg-type 'const (lambda (config) '(0 1 -1 -2 -8)))
    (define-arg-type 'bit (lambda (config) '(0 1)))

    ;; Inform GreenThumb how many opcodes there are in one instruction.
    (init-machine-description 1)
    
    (define-instruction-class 'nop '(nop))
    
    (define-instruction-class 'pr '(not)
      #:args '(pred reg) #:ins '(1) #:outs '(0))
    
    (define-instruction-class 'rr '(mov)
      #:args '(reg reg) #:ins '(1) #:outs '(0))

    ;; An example of an instruction that takes two input registers
    ;; and update one output register
    (define-instruction-class 'rrr-commute '(add)
      #:args '(reg reg reg) #:ins '(1 2) #:outs '(0) #:commute '(1 . 2))
    
    (define-instruction-class 'rrr '(sub)
      #:args '(reg reg reg) #:ins '(1 2) #:outs '(0))

    ;; An example of an instruction that takes an input register and a constant
    ;; and update one output register.
    ;; Notice that opcodes in different classes can't have the same name.
    (define-instruction-class 'rri '(add# mul.lo# and# rem#)
      #:args '(reg reg const) #:ins '(1 2) #:outs '(0))
    
    (define-instruction-class 'prb '(setp.eq#)
      #:args '(pred reg bit) #:ins '(1 2) #:outs '(0))

    (define-instruction-class 'rrri '(mul.wide#)
      #:args '(reg reg reg const) #:ins '(2 3) #:outs '(0 1))

    ;; An example of an instruction that takes an input register and a shift constant
    ;; and update one output register
    (define-instruction-class 'rrb '(shl# shr#)
      #:args '(reg reg bit) #:ins '(1) #:outs '(0))

    (define-instruction-class 'rrrp '(selp)
      #:args '(reg reg reg pred) #:ins '(1 2 3) #:outs '(0))

    ;; An example of an instruction that accesses memory
    #;(define-instruction-class 'load '(load)
      #:args '(reg reg) #:ins (list 1 (get-memory-type)) #:outs '(0))

    ;; An example of an instruction that updates memory
    #;(define-instruction-class 'store '(store)
      #:args '(reg reg) #:ins '(0 1) #:outs (list (get-memory-type)))
    

    (finalize-machine-description)

    ;;;;;;;;;;;;;;;;;;;;;;;;; For enumerative search ;;;;;;;;;;;;;;;;;;;;;;;
    #|
    ;; This function is used as part of enumerative search when it executes load instruction backward.
    ;; GreenThumb automatically looks up a possible address (addr) from a memory object.
    ;;
    ;; Implement this function to inform about the order of inputs for load instruction
    ;; (as defined with #:ins) so that GreenThumb can put the address and memory object
    ;; to the right locations in a program state.
    (define/override (update-progstate-ins-load my-inst addr mem state)
      ? ;; modify this function

      ;; Example:
      ;; Put addr before mem  => input 0 is addr, input 1 is memory.
      ;; If store instruction takes more than one input argument for address, we have to do more work.
      ;; See arm-machine.rkt for example.
      (update-progstate-ins my-inst (list addr mem) state))
    
    ;; This function is used as part of enumerative search when it executes store instruction backward.
    ;; GreenThumb automatically looks up a possible address (addr) and a stored value (val)
    ;; from a memory object.
    ;;
    ;; Implement this function to inform about the order of inputs for store instruction
    ;; (as defined with #:ins) so that GreenThumb can put the address and value
    ;; to the right locations in a program state.
    (define/override (update-progstate-ins-store my-inst addr val state)
      ? ;; modify this function

      ;; Example:
      ;; Put val before addr => input 0 is val, input 1 is address.
      ;; If load instruction takes more than 2 input arguments, we have to do more work.
      ;; See arm-machine.rkt for example.
      (update-progstate-ins my-inst (list val addr) state))
    |#

    ))
      

