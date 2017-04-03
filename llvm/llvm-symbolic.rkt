#lang s-exp rosette

(require "../symbolic.rkt" "../inst.rkt")

(provide llvm-symbolic%)

(define llvm-symbolic%
  (class symbolic%
    (super-new)
    (inherit sym-op sym-arg)
    (inherit-field machine)
    (override len-limit gen-sym-inst)

    ;; Num of instructions that can be synthesized within a minute.
    (define (len-limit) 3)
    
    ;; Create symbolic operand using Rosette symbolic variable.
    (define (sym-arg-vec)
      (define-symbolic* is-num boolean?)
      (if is-num
          (sym-arg)
          (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg))))

    (define (gen-sym-inst)
      (define args (get-field max-number-of-args machine))
      (inst (sym-op)
            (vector-append (vector (sym-arg))
                           (for/vector ([arg (sub1 args)]) (sym-arg-vec)))))
    
    ))
