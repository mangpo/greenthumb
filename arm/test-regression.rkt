#lang s-exp rosette

(require "arm-validator.rkt" "arm-machine.rkt" "arm-printer.rkt"
         "arm-parser.rkt" "../inst.rkt"
         "arm-simulator-racket.rkt" "arm-simulator-rosette.rkt"
         "arm-symbolic.rkt" "arm-stochastic.rkt" "arm-forwardbackward.rkt"
         "arm-inverse.rkt" "arm-enumerator.rkt")


(define parser (new arm-parser%))
(define machine (new arm-machine% [config 4]))

(define printer (new arm-printer% [machine machine]))
(define simulator-racket (new arm-simulator-racket% [machine machine]))
(define simulator-rosette (new arm-simulator-rosette% [machine machine]))
(define validator (new arm-validator% [machine machine] [simulator simulator-rosette]))

(define symbolic (new arm-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))
(define backward (new arm-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% arm-inverse%]
                      [enumerator% arm-enumerator%]
                      [syn-mode `linear]))

(define (test id code-str size liveout-str #:sym [sym #f] #:enum [enum #t] #:assume [assume #f]
              #:cost [cost #f])
  (pretty-display (format "TEST = ~a" id))
  (define code (send parser ir-from-string code-str))
  (define encoded-code (send printer encode code))
  (send validator adjust-memory-config encoded-code)
  
  (define sketch (for/vector ([i size]) (send symbolic gen-sym-inst)))
  (define constraint (send printer encode-live liveout-str))
  
  ;; symbolic
  (when sym
    (define-values (out-sym cost-sym)
      (send symbolic synthesize-from-sketch encoded-code sketch constraint cost
            #:assume (and assume (send machine constrain-stack assume))))
    
    (unless out-sym (raise (format "TEST ~a: fail to synthesize [symbolic]" id)))
    (define ce-sym (send validator counterexample encoded-code out-sym constraint
                         #:assume (and assume (send machine constrain-stack assume))))
    (when ce-sym (raise (format "TEST ~a: counter-example [symbolic]" id))))

  (when enum
    (define out-enum
      (send backward synthesize-window
            encoded-code
            sketch
            (vector) (vector)
            constraint ;; live-out
            cost ;; upperbound cost, #f = no upperbound
            3600 ;; time limit in seconds
            #:assume (and assume (send machine constrain-stack assume))
            ))
    
    (unless out-enum (raise (format "TEST ~a: fail to synthesize [enumerative]" id)))
    (define ce-enum (send validator counterexample encoded-code out-enum constraint
                          #:assume (and assume (send machine constrain-stack assume))))
    (when ce-enum (raise (format "TEST ~a: counter-example [enumerative]" id))))
  )

(test 'sym "
rsb r1, r0, r0, lsr 1
" 1 '(1) #:sym #t)

(test 'p13 "
	rsb	r3, r0, #0
	mov	r0, r0, asr #31
	orr	r0, r0, r3, asr #31
" 2 '(0) #:cost 3)

(test 'p14 "
        eor     r3, r1, r0
        and     r0, r1, r0
        add     r0, r0, r3, asr #1
" 3 '(0))

(test 'p15 "
        orr     r3, r1, r0
        eor     r0, r1, r0
        sub     r0, r3, r0, asr #1
" 3 '(0))

(test 'p16 "
        cmp     r0, r1
        movcc   r0, r1
" 2 '(0) #:sym #t)

(test 'p16-v2 "
        add r0, r0, #1
        cmp     r0, r1
        movcc   r0, r1
" 3 '(0))

(test 'p16-v3 "
        cmp     r0, r1
        movcc   r0, r1
        str r0, [r2, #0]
" 2 '(0))


(test 'p16-v4 "
        cmp     r0, r1
        movcc   r0, r1
        str r0, [r2, #0]
" 3 '(memory)) ;; fail

(test 'store1 "
        eor     r3, r1, r0
        and     r0, r1, r3
        str r0, [r2, #0]
" 3 '(memory))

(test 'store2 "
        cmp     r0, r1
        movcc   r0, r3
        strne r0, [r2, #0]
" 3 '(memory))

(test 'load1 "
        ldr    r0, [r2, #0]
        eor     r3, r1, r0
        and     r0, r1, r3
" 2 '(0))

(test 'load2 "
        cmp     r0, r1
        movcc   r0, r1
        ldreq r0, [r2, #0]
" 3 '(0))

;; 160 s
#;(test 1 "
	sub	r3, r0, #1
	tst	r3, r0
	movne	r3, #0
	moveq	r3, #1
	cmp	r0, #0
	moveq	r0, #0
	andne	r0, r3, #1" 3 '(0))
