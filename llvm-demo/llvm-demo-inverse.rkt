#lang racket

(require "../ops-racket.rkt" "../inst.rkt" "../inverse.rkt" "../enumerator.rkt")

(provide llvm-demo-inverse%)

(define llvm-demo-inverse%
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

    (define (uid-inst-in-out x)
      (define-values (i in out) (super uid-inst-in-out x))
      (values i in (car out)))

    ;; Inverse tables for all instructions.
    (define behaviors-bw (make-hash))

    ;; Generate inverse table behavior for my-inst.
    (define (gen-inverse-behavior my-inst)
      (define opcode (inst-op my-inst))
      (define opcode-name (vector-ref opcodes opcode))
      (define args (inst-args my-inst))
      ;; For collecting which registers in my-inst are input and output.
      (define in (make-vector 5 #f))
      (define out-reg (vector-ref args 0))
      ;; Inverse table behavior
      (define behavior-bw (make-hash))
      ;; Collect information on which registers are input and output.
      (for ([arg args]
	    [type (send machine get-arg-types opcode-name)])
	   (when (equal? type `var-i) (vector-set! in arg val-range)))

      (for ([in-res (all-combination-list (vector->list in))])
           (let ([out-state
                  (with-handlers*
                   ([exn? (lambda (e) #f)])
                   (send simulator interpret
                         (vector my-inst) (list->vector in-res)))])
          
             (when 
              out-state
              (define in-list-filtered (filter number? in-res))
              (define key2 (vector-ref out-state out-reg))

              ;; Insert into the inverse table. 
              (hash-insert-to-list behavior-bw key2 in-list-filtered))))
      
      (define-values (key1 vars-in var-out) (uid-inst-in-out my-inst))
      ;;(pretty-display `(behavior-bw ,behavior-bw))
      (hash-set! behaviors-bw key1 behavior-bw))
    
    ;; Inverse interpret my-inst using the pre-computed 'behaviors-bw'.
    ;; my-inst: instruction
    ;; state-vec: progstate in vector/list/pair format
    ;; old-liveout: liveout info
    ;; output: a list of progstates in vector/list/pair format
    (define (interpret-inst my-inst state old-liveout)
      (define opcode-name (vector-ref opcodes (inst-op my-inst)))
      (define n (vector-length state))

      (define-values (key1 vars-in var-out) (uid-inst-in-out my-inst))
      
      ;; Extract values of var-out from state.
      ;; This corresponds to key2 in gen-inverse-behavior.
      ;; It will be used as key2 to the same table.
      (define key2 (vector-ref state var-out))
      
      ;; A vector for initializing a vector containing values of variables in the input state.
      (define state-base (make-vector n #f))
      (for ([i n]
	    [l old-liveout])
	   (when (and l (not (= i var-out)))
		 (vector-set! state-base i (vector-ref state i))))

      (define mapping (hash-ref behaviors-bw key1))
      (lookup-bw mapping vars-in key2 state-base)
      )

    ))
