#lang s-exp rosette

(require "../symbolic.rkt" 
	 "../ast.rkt"
	 "GA-simulator-rosette.rkt" "GA-validator.rkt")

(provide GA-symbolic%)

(define GA-symbolic%
  (class symbolic%
    (super-new)
    (inherit-field machine printer simulator validator)
    (override len-limit window-size)

    (define (len-limit) 9)
    (define (window-size) 14)
    (set! simulator (new GA-simulator-rosette% [machine machine]))
    (set! validator (new GA-validator% [machine machine] [printer printer] [simulator simulator]))

    ))
