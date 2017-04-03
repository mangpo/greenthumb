#lang s-exp rosette

(require "../symbolic.rkt" 
	 "../inst.rkt")

(provide arm-symbolic%)

(define arm-symbolic%
  (class symbolic%
    (super-new)
    (override len-limit window-size)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 2)

    ;; Context-aware window decomposition size L.
    ;; The cooperative search tries L/2, L, 2L, 4L.
    (define (window-size) 4)

    ))
