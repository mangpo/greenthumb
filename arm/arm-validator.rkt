#lang s-exp rosette

(require "../validator.rkt"
         "../ast.rkt" "arm-ast.rkt")
(provide arm-validator%)

(define arm-validator%
  (class validator%
    (super-new)
    (inherit-field printer)
    (inherit sym-op sym-arg encode-sym)
    (override encode-sym-inst) 

    ;; Encode instruction x.
    ;; If x is a concrete instruction, then encode textual representation using number.
    ;; If (inst-op x) = #f, create symbolic instruction.
    (define (encode-sym-inst x)
      (if (inst-op x)
          (send printer encode-inst x)
          (arm-inst (sym-op) 
                    (vector (sym-arg) (sym-arg) (sym-arg) (sym-arg))
                    (sym-op)
                    (sym-arg)
                    (sym-op))))

    ))
