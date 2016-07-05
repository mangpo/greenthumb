#lang racket

(require "../compress.rkt" "../inst.rkt")

(provide llvm-demo-compress%)

(define llvm-demo-compress%
  (class compress%
    (super-new)
    (inherit-field machine)
    (override compress-reg-space)
    
    ;; Input
    ;; program: string IR format
    ;; Output
    ;; 1) compressed program in the same format as input
    ;; 2) compressed live-out
    ;; 3) compressed live-in
    ;; 4) map-back
    ;; 5) machine-info in custom format---nvars for llvm
    (define (compress-reg-space program live-out live-in)
      (define vars (list))
      (for* ([x program]
	     [arg (inst-args x)])
	    (when (and (equal? "%" (substring arg 0 1))
		       (not (member arg vars)))
		  (set! vars (cons arg vars))))
      (values program live-out live-in #f (add1 (length vars))))

    ))
      
