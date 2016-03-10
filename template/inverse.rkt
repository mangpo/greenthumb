#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt" "../enumerator.rkt")

(provide $-inverse%)

(define $-inverse%
  (class inverse%
    (super-new)
    (inherit-field machine simulator)
    (inherit lookup-bw)
    (override gen-inverse-behavior interpret-inst uid-inst-in-out)

    ;; Reduced-bit
    (define bit (get-field bitwidth machine))
    (define opcodes (get-field opcodes machine))
    (define val-range
      (for/vector ([v (arithmetic-shift 1 bit)]) (finitize v bit)))

    ;; Inverse tables for all instructions.
    (define behaviors-bw (make-hash))

    ;; Modify this method for different forms of UIDs.
    ;; 
    ;; Default:
    ;; Return a unique representation for
    ;;  i) an instruction including opcode and constant arguments (list)
    ;; and extract IDs of
    ;;  ii) input arguments (list)
    ;;  iii) output argument (list)
    (define (uid-inst-in-out x)
      (define-values (inst-key in out) (super uid-inst-in-out x))
      (values ? ? ?))

    
    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      ;; Inverse table behavior
      (define behavior-bw (make-hash))
      ?
      (define-values (key1 vars-in vars-out) (uid-inst-in-out my-inst))
      (hash-set! behaviors-bw key1 behavior-bw))

    
    ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; output: a list of program states
    (define (interpret-inst my-inst state liveout)
      (define-values (key1 vars-in vars-out) (uid-inst-in-out my-inst))
      ;; Get inverse behavior of uid-inst
      (define mapping (hash-ref behaviors-bw key1))
      ?
      )

    ))
