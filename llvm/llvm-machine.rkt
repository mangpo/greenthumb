#lang racket

(require "../machine.rkt" "../inst.rkt" "../special.rkt")

(provide llvm-machine% (all-defined-out))

(define llvm-machine%
  (class machine%
    (super-new)
    (inherit-field bitwidth random-input-bits config)
    (inherit define-instruction-class finalize-machine-description
             define-progstate-type define-arg-type
             update-progstate-ins kill-outs)
    (override get-constructor set-config progstate-structure 
              update-progstate-ins-load
              update-progstate-ins-store)

    (define (get-constructor) llvm-machine%)
    
    (unless bitwidth (set! bitwidth 32))
    (set! random-input-bits bitwidth)

    (when config (set-config config))
    
    (define (set-config x) 
      (set! config x))

    ;;;;;;;;;;;;;;;;;;;;; program state ;;;;;;;;;;;;;;;;;;;;;;;;

    (define (progstate-structure)
      (vector (for/vector ([i config]) 'var)
              (get-memory-type)))

    (define-progstate-type
      'var 
      #:get (lambda (state arg) (vector-ref (vector-ref state 0) arg))
      #:set (lambda (state arg val) (vector-set! (vector-ref state 0) arg val)))

    (define-progstate-type
      (get-memory-type)
      #:get (lambda (state) (vector-ref state 1))
      #:set (lambda (state val) (vector-set! state 1 val)))

    ;;;;;;;;;;;;;;;;;;;;; instruction classes ;;;;;;;;;;;;;;;;;;;;;;;;
    (define-arg-type 'var (lambda (config) (range config)))
    (define-arg-type 'const (lambda (config) '(0 1 -1 -2 -8)))
    (define-arg-type 'bit (lambda (config) '(0 1)))
    
    (define-instruction-class 'nop '(nop))

    (define-instruction-class
     'rrr-commute
     '(and or xor add)
     #:args '(var var var)
     #:ins '(1 2)
     #:outs '(0)
     #:commute '(1 . 2)
     )

    (define-instruction-class
     'rrr
     '(sub shl lshr ashr)
     #:args '(var var var)
     #:ins '(1 2)
     #:outs '(0))

    (define-instruction-class
     'rri
     '(and# or# xor# add# sub#)
     #:args '(var var const)
     ;; Input arguments that related to program state + additional input related to program state
     ;; Exclude the 3rd argument because it is not related to program state.
     #:ins '(1)   
     #:outs '(0))

    (define-instruction-class
     'rrb
     '(shl# lshr# ashr#)
     #:args '(var var bit)
     #:ins '(1)
     #:outs '(0))

    (define-instruction-class
     'rir
     '(_sub _shl _lshr _ashr)
     #:args '(var const var)
     #:ins '(2)
     #:outs '(0))

    (define-instruction-class
     'rir
     '(_sub _shl _lshr _ashr)
     #:args '(var const var)
     #:ins '(2)
     #:outs '(0))

    (define-instruction-class
     'rr
     '(ctlz)
     #:args '(var var)
     #:ins '(1)
     #:outs '(0))

    (define-instruction-class
     'load
     '(load)
     #:args '(var var)
     #:ins (list 1 (get-memory-type))
     #:outs '(0))

    (define-instruction-class
     'store
     '(store)
     #:args '(var var)
     #:ins '(0 1)
     #:outs (list (get-memory-type)))

    (finalize-machine-description)

    ;; Inform about the order of argument for load instruction
    (define (update-progstate-ins-load my-inst addr mem state)
      (define state-base (kill-outs my-inst state))
      (update-progstate-ins my-inst (list addr mem) state-base))

    ;; Inform about the order of argument for store instruction
    (define (update-progstate-ins-store my-inst addr val state)
      ;; Put val before addr => arg 0 is val, arg 1 is address.
      (update-progstate-ins my-inst (list val addr) state))

    ))
      
