#lang s-exp rosette

(require "../validator.rkt"
         "../ast.rkt" "arm-ast.rkt"
         "../machine.rkt"
         "arm-machine.rkt" "arm-printer.rkt" "arm-simulator-rosette.rkt")
(provide arm-validator%)

(define arm-validator%
  (class validator%
    (super-new)
    (inherit-field printer machine simulator)
    (inherit sym-op sym-arg encode-sym)
    (override encode-sym-inst) 

    (set! printer (new arm-printer% [machine machine]))
    (set! simulator (new arm-simulator-rosette% [machine machine]))

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
