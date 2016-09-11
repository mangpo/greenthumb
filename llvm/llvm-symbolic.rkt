#lang s-exp rosette

(require "../symbolic.rkt" "../inst.rkt")

(provide llvm-symbolic%)

(define llvm-symbolic%
  (class symbolic%
    (super-new)
    (inherit sym-op sym-arg)
    (override len-limit gen-sym-inst)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 3)
    
    (define (gen-sym-inst)
      (inst (sym-op) (vector (sym-arg) (sym-arg) (sym-arg))))
    ))
