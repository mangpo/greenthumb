#lang s-exp rosette

(require "../symbolic.rkt" 
	 "../ast.rkt" "arm-ast.rkt")

(provide arm-symbolic%)

(define arm-symbolic%
  (class symbolic%
    (super-new)
    (override len-limit window-size evaluate-inst)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 2)

    ;; Context-aware window decomposition size L.
    ;; The cooperative search tries L/2, L, 2L, 4L.
    (define (window-size) 4)

    ;; Evaluate a symbolic instruction to a concrete instruction according to a given model.
    (define (evaluate-inst x model)
      (arm-inst (evaluate (inst-op x) model)
                (vector-map 
                 (lambda (a) (evaluate a model)) (inst-args x))
                (evaluate (inst-shfop x) model)
                (evaluate (inst-shfarg x) model)
                (evaluate (inst-cond x) model)))

    ))
