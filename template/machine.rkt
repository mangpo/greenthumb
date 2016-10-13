#lang racket

(require "../machine.rkt" "../special.rkt")

(provide $-machine%  (all-defined-out))

;;;;;;;;;;;;;;;;;;;;; program state macro ;;;;;;;;;;;;;;;;;;;;;;;;
;; This is just for convenience.
(define-syntax-rule
  (progstate regs memory)
  (vector regs memory))

(define-syntax-rule (progstate-regs x) (vector-ref x 0))
(define-syntax-rule (progstate-memory x) (vector-ref x 1))

(define-syntax-rule (set-progstate-regs! x v) (vector-set! x 0 v))
(define-syntax-rule (set-progstate-memory! x v) (vector-set! x 1 v))

(define $-machine%
  (class machine%
    (super-new)
    (inherit-field bitwidth random-input-bits config)
    (inherit init-machine-description define-instruction-class finalize-machine-description
             define-progstate-type define-arg-type
             update-progstate-ins kill-outs)
    (override get-constructor progstate-structure 
              ;; >> required fileds for stochastic and enumerative only
              update-progstate-ins-load
              update-progstate-ins-store)

    (define (get-constructor) $-machine%)
    
    (unless bitwidth (set! bitwidth ?))
    (set! random-input-bits bitwidth)

    ;;;;;;;;;;;;;;;;;;;;; program state ;;;;;;;;;;;;;;;;;;;;;;;;

    (define (progstate-structure)
      (progstate (for/vector ([i config]) 'reg)
                 (get-memory-type)))

    (define-progstate-type
      'reg 
      #:get (lambda (state arg) (vector-ref (progstate-regs state) arg))
      #:set (lambda (state arg val) (vector-set! (progstate-regs state) arg val)))

    (define-progstate-type
      (get-memory-type)
      #:get (lambda (state) (progstate-memory state))
      #:set (lambda (state val) (set-progstate-memory! state val)))

    ;;;;;;;;;;;;;;;;;;;;; instruction classes ;;;;;;;;;;;;;;;;;;;;;;;;
    (define-arg-type 'reg (lambda (config) (range config)))
    (define-arg-type 'const (lambda (config) '(0 1 -1 -2 -8)))
    (define-arg-type 'bit (lambda (config) '(0 1)))
    ;; try more values for const than for bit

    ;; Inform GreenThumb how many opcodes there are in one instruction.
    (init-machine-description 1)
    
    (define-instruction-class 'nop '(nop))

    ;; An example of an instruction that takes two input registers
    ;; and update one output register
    (define-instruction-class 'rrr-commute '(add)
     #:args '(reg reg reg) #:ins '(1 2) #:outs '(0) #:commute '(1 . 2))

    ;; An example of an instruction that takes an input register and a constant
    ;; and update one output register.
    ;; Notice that opcodes in different classes can't have the same name.
    (define-instruction-class 'rri '(add#)
     #:args '(reg reg const) #:ins '(1 2) #:outs '(0))

    ;; An example of an instruction that takes an input register and a shift constant
    ;; and update one output register
    (define-instruction-class 'rrb '(shl#)
     #:args '(reg reg bit) #:ins '(1) #:outs '(0))

    ;; An example of an instruction that accesses memory
    (define-instruction-class 'load '(load)
     #:args '(reg reg) #:ins (list 1 (get-memory-type)) #:outs '(0))

    ;; An example of an instruction that updates memory
    (define-instruction-class 'store '(store)
     #:args '(reg reg) #:ins '(0 1) #:outs (list (get-memory-type)))

    (finalize-machine-description)

    ;; Inform about the order of argument for store instruction.
    (define (update-progstate-ins-store my-inst addr val state)
      ;; Put val before addr => arg 0 is val, arg 1 is address.
      (update-progstate-ins my-inst (list val addr) state))

    ;; Inform about the order of argument for load instruction.
    ;; Need to do more work if load instruction takes more than one input argument.
    (define (update-progstate-ins-load my-inst addr mem state)
      (define state-base (kill-outs my-inst state))
      (update-progstate-ins my-inst (list addr mem) state-base))

    ))
      
