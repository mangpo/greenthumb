#lang s-exp rosette

(require "../symbolic.rkt" "../ast.rkt")

(provide llvm-demo-symbolic%)

(define llvm-demo-symbolic%
  (class symbolic%
    (super-new)
    (inherit sym-op sym-arg)
    (override len-limit window-size gen-sym-inst)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 2)

    ;; Context-aware window decomposition size L.
    ;; The cooperative search tries L/2, L, 2L, 4L.
    (define (window-size) 4)
    
    (define (gen-sym-inst)
      (inst (sym-op) (vector (sym-arg) (sym-arg) (sym-arg))))
    ))
