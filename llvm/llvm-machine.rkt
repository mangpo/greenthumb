#lang racket

(require "../machine.rkt" "../special.rkt")

(provide llvm-machine% (all-defined-out))

;; define progstate macro
(define-syntax-rule
  (progstate var vec4 mem)
  (vector var vec4 mem))

(define-syntax-rule (progstate-var x) (vector-ref x 0))
(define-syntax-rule (progstate-vec4 x) (vector-ref x 1))
(define-syntax-rule (progstate-memory x) (vector-ref x 2))

(define-syntax-rule (set-progstate-var! x v) (vector-set! x 0 v))
(define-syntax-rule (set-progstate-vec4! x v) (vector-set! x 1 v))
(define-syntax-rule (set-progstate-memory! x v) (vector-set! x 2 v))

(define llvm-machine%
  (class machine%
    (super-new)
    (inherit-field bitwidth random-input-bits config argtypes-info)
    (inherit init-machine-description define-instruction-class finalize-machine-description
             define-progstate-type define-arg-type
             update-progstate-ins kill-outs)
    (override get-constructor progstate-structure 
              ;; >> required fileds for stochastic and enumerative only
              update-progstate-ins-load
              update-progstate-ins-store)

    (define (get-constructor) llvm-machine%)
    
    (unless bitwidth (set! bitwidth 32))
    (set! random-input-bits bitwidth)

    ;;;;;;;;;;;;;;;;;;;;; program state ;;;;;;;;;;;;;;;;;;;;;;;;

    (define (progstate-structure)
      (progstate (for/vector ([i (car config)]) 'var)
                 (for/vector ([i (cdr config)]) 'vec4)
                 (get-memory-type)))

    ;; keep track of liveness at the level of program state element unit
    (define-progstate-type
      'var 
      #:get (lambda (state arg) (vector-ref (progstate-var state) arg))
      #:set (lambda (state arg val) (vector-set! (progstate-var state) arg val)))

    (define-progstate-type
      'vec4
      #:structure (for/vector ([i 4]) 'x) ;; a vector contains 4 primitive elements
      #:get (lambda (state arg) (vector-ref (progstate-vec4 state) arg))
      #:set (lambda (state arg val) (vector-set! (progstate-vec4 state) arg val)))

    (define-progstate-type
      (get-memory-type)
      #:get (lambda (state) (progstate-memory state))
      #:set (lambda (state val) (set-progstate-memory! state val)))

    ;;;;;;;;;;;;;;;;;;;;; instruction classes ;;;;;;;;;;;;;;;;;;;;;;;;
    (define-arg-type 'var (lambda (config) (range (car config))))
    (define-arg-type 'vec4 (lambda (config) (range (cdr config))))
    (define-arg-type 'const (lambda (config) '(0 1 3 -1 -2 -8)))
    (define-arg-type 'bit (lambda (config) '(0 1)))
    (define-arg-type 'const-vec4
      (lambda (config) (list (vector 0 0 0 0)
                             (vector 1 1 1 1))))
    
    (init-machine-description 1)
    (define-instruction-class 'nop '(nop))

    (define-instruction-class
     'rrr-commute
     '(and or xor add mul)
     #:args '(var var var)
     #:ins '(1 2)
     #:outs '(0)
     #:commute '(1 . 2)
     )

    (define-instruction-class
     'rrr-commute-vec4
     '(add_v4)
     #:scalar '(add) #:vector-width 4
     #:args '(vec4 vec4 vec4)
     #:ins '(1 2)
     #:outs '(0)
     #:commute '(1 . 2)
     )

    (define-instruction-class
     'rrr
     '(sub shl lshr ashr udiv sdiv urem srem)
     #:args '(var var var)
     #:ins '(1 2)
     #:outs '(0))

    (define-instruction-class
     'rri
     '(and# or# xor# add# sub# mul# udiv# sdiv# urem# srem#)
     #:args '(var var const)
     ;; Input arguments that related to program state + additional input related to program state
     ;; Exclude the 3rd argument because it is not related to program state.
     #:ins '(1)   
     #:outs '(0))

    (define-instruction-class
     'rri-vec4
     '(add_v4#)
     #:scalar '(add#) #:vector-width 4
     #:args '(vec4 vec4 const-vec4)
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
     '(_sub _shl _lshr _ashr _udiv _sdiv _urem _srem)
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
      ;; Put addr before mem  => input 0 is addr, input 1 is memory.
      (update-progstate-ins my-inst (list addr mem) state))

    ;; Inform about the order of argument for store instruction
    (define (update-progstate-ins-store my-inst addr val state)
      ;; Put val before addr => input 0 is val, input 1 is address.
      (update-progstate-ins my-inst (list val addr) state))

    ;; Include all numbers in const-vec4 in const and vice.
    ;; In x is in const, then (vector x x x x) is in const-vec4.
    (define/override (analyze-args prefix code postfix live-in-list live-out)
      (super analyze-args prefix code postfix live-in-list live-out)
      (define const-vec4 (hash-ref argtypes-info 'const-vec4))
      (define const (hash-ref argtypes-info 'const))

      (define const-vec4-unique (list->set (flatten (map vector->list (argtype-valid const-vec4)))))
      (define const-vec4-list (argtype-valid const-vec4))
      (define const-list (argtype-valid const))

      (for ([c const-list])
           (let ([vec (vector c c c c)])
           (unless (member vec const-vec4-list)
                   (set! const-vec4-list (cons vec const-vec4-list)))))

      (for ([c const-vec4-unique])
           (unless (member c const-list)
                   (set! const-list (cons c const-list))))

      (set-argtype-valid! const-vec4 const-vec4-list)
      (set-argtype-valid! const const-list)

      (when #t
            (pretty-display `(analyze-args))
            (for ([pair (hash->list argtypes-info)])
                 (let ([name (car pair)]
                       [info (cdr pair)])
                   (pretty-display `(ARG ,name ,(argtype-valid info))))))
      )

    ))
      
