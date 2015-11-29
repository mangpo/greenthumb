#lang racket

(require "../machine.rkt" "../inst.rkt")

(provide $-machine%)

(define $-machine%
  (class machine%
    (super-new)
    (inherit-field bit random-input-bit config
                   inst-id nop-id
                   ;; >> required fileds for stochastic and enumerative only
		   ;; classes
                   )
    (inherit get-class-id filter-live)
    (override get-constructor set-config get-state
              ;; >> Required methods for stochastic and enumerative only
              ;; reset-arg-ranges get-arg-ranges update-live update-live-backward
              )

    (define (get-constructor) $-machine%)
    
    (unless bit (set! bit ?))
    (set! random-input-bit bit)
    (set! nop-id 0)
    (set! inst-id '#(nop)) ;; list of instructions

    (when config (set-config config))

    ;; Save program state parameters to appropriate fields.
    (define (set-config config)
      ?
      (reset-arg-ranges))
    
    ;; Generate a program state from lambda init.
    (define (get-state init [extra #f]) ?)

    ;;;;;;;;;;;;;;;;;;;;; For stochastic and enumerative ;;;;;;;;;;;;;;;;;;

    ;; Set valid operands' ranges from saved config parameters.
    (define (reset-arg-ranges) (void))
    
    #|
    ;; Instruction classes
    (set! classes 
          (vector '() ;; list of type 1 instructions
        	  '() ;; list of type 2 instructions
                  ))

    ;; Get valid operands' values given opcode-name, live-in, live-out, and mode.
    ;; >> INPUT >>
    ;; opcode-name: opcode in symbol form (e.g. `nop)
    ;; live-in & live-out: liveness infomation
    ;; mode: can be either
    ;;  1) `basic (no restriction)
    ;;  2) `no-args = ignore register/variable operands. Return a symbol like `var-i and `var-o instead of a vector of values for register/varaible operands. The enumerator% (which will be extended later) used in enumerative search will use this.
    ;; >> OUTPUT >>
    ;; A vector of vectors containing valid values for operands
    (define (get-arg-ranges opcode-name entry live-in
                            #:live-out [live-out #f] #:mode [mode `basic])
      ?)

    ;; Return live-out
    (define (update-live live-in this-inst)
      ?)

    ;; Return live-in (for enumerative search).
    (define (update-live-backward live-out this-inst)
      ?)
    
    ))
      
