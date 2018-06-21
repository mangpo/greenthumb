#lang s-exp rosette

(require "../inst.rkt"
         "ptx-parser.rkt" "ptx-machine.rkt" "ptx-printer.rkt"
         "ptx-simulator-rosette.rkt" 
         "ptx-simulator-racket.rkt"
         "ptx-validator.rkt"
         "ptx-symbolic.rkt"
         "ptx-stochastic.rkt"
         "ptx-forwardbackward.rkt" "ptx-enumerator.rkt" "ptx-inverse.rkt"
         )

(define parser (new ptx-parser%))
(define machine (new ptx-machine% [config (cons 4 2)]))
(define printer (new ptx-printer% [machine machine]))
(define simulator-racket (new ptx-simulator-racket% [machine machine]))
(define simulator-rosette (new ptx-simulator-rosette% [machine machine]))
(define validator (new ptx-validator% [machine machine] [simulator simulator-rosette]))


(define prefix 
(send parser ir-from-string "
"))

(define postfix
(send parser ir-from-string "
"))

(define code
(send parser ir-from-string "

and.u32 %r0, %r0, 31;
	setp.eq.b32	%p0, %r0, 0;
not.pred %p0, %p0;
"))

(define sketch
(send parser ir-from-string "
?
?
?
"))

(define constraint (progstate (vector #f #t #f #f) (vector #t #f)))

(define code1
(send parser ir-from-string "
and.u32 %r0, %r0, 31;
	mul.wide.u32 	%r2, %r1, %r0, -1431655765;
	shr.u32 	%r1, %r1, 1;
	mul.lo.s32 	%r1, %r1, 3;
	sub.s32 	%r1, %r0, %r1;
"))

(define sketch1
(send parser ir-from-string "
?
?
"))

;; very slow if include p0
;; and.u32 %r0, %r0, 31;
;; rem.u32 


(define constraint1 (progstate (vector #t #t #f #f) (vector #f #f)))

(define code2
(send parser ir-from-string "
	setp.eq.b32	%p0, %r0, 0;
	selp.b32	%r1, %r2, %r3, %p0;
"))

(define sketch2
(send parser ir-from-string "
	setp.eq.b32	%p0, %r0, 0;
?
selp.b32 %r1, %r3, %r2, %p0;
"))


(define constraint2 (progstate (vector #t #t #f #f) (vector #f)))

(define code3
(send parser ir-from-string "
add.s32 %r0, %r0, 1;
add.s32 %r1, %r0, %r0;
"))

(define sketch3
(send parser ir-from-string "
shl.s32 %r1, %r0, 1;
?
"))


(define constraint3 (progstate (vector #f #t #f #f) (vector #f #f)))

(define encoded-code (send printer encode code))
(define encoded-sketch (send printer encode sketch))
(define encoded-prefix (send printer encode prefix))
(define encoded-postfix (send printer encode postfix))

(send validator adjust-memory-config
      (vector-append encoded-prefix encoded-code encoded-postfix))


;; Phase A: create symbolic search (step 4)
(define symbolic (new ptx-symbolic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]))

#;(send symbolic synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

;; Phase B: create stochastic search (step 5)
(define stoch (new ptx-stochastic% [machine machine]
                      [printer printer] [parser parser]
                      [validator validator] [simulator simulator-rosette]
                      [syn-mode #t] ;; #t = synthesize, #f = optimize mode
                      ))
#;(send stoch superoptimize encoded-code 
      constraint ;; live-out
      "./driver-0" 
      3600 ;; time limit in seconds
      #f ;; size limit
      )


;; Phase C: create enumerative search (step 6)
(define backward (new ptx-forwardbackward% [machine machine] 
                      [printer printer] [parser parser] 
                      [validator validator] [simulator simulator-racket]
                      [inverse% ptx-inverse%]
                      [enumerator% ptx-enumerator%]
                      [syn-mode `linear]))
(send backward synthesize-window
      encoded-code
      encoded-sketch
      encoded-prefix encoded-postfix
      constraint ;; live-out
      #f ;; upperbound cost, #f = no upperbound
      3600 ;; time limit in seconds
      )

