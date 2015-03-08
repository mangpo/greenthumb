#lang s-exp rosette

(require "../symbolic.rkt" 
	 "../ast.rkt" "neon-ast.rkt" 
	 "neon-simulator-rosette.rkt" "neon-validator.rkt")
(provide neon-symbolic%)

(define neon-symbolic%
  (class symbolic%
    (super-new)
    (inherit-field machine printer simulator validator)
    (override len-limit window-size evaluate-inst)

    (define (len-limit) 1)
    (define (window-size) 2)
    (set! simulator (new neon-simulator-rosette% [machine machine]))
    (set! validator (new neon-validator% [machine machine] [printer printer]))

    (define (evaluate-inst x model)
      (neon-inst (evaluate (inst-op x) model)
                 (vector-map (lambda (a) (evaluate a model)) (inst-args x))
                 (evaluate (inst-byte x) model)
                 (evaluate (inst-type x) model)))

    ))